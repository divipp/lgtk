{-# LANGUAGE RankNTypes #-}
module Data.MLens.Ref
    ( Unit

    -- * Data type for reference lenses
    , Ref (..)

    -- * Reference operations
    , (%)
    , mapRef
    , unitRef
    , runRef
    , mkRef
    , readRef, writeRef, modRef
    , joinRef

    -- * Some impure @IO@ referenceses
    , fileRef, fileRef_

    -- * Monadic lenses data type
    , Lens

    -- * Lens construction
    , lens

    -- * Lens operations
    , getL, setL, modL

    -- * Pure lenses
    , unitLens
    ) where

import qualified Control.Arrow as Arrow
import Control.Monad.Identity
import Data.Maybe
import Data.Lens.Common hiding (getL, setL, modL)
import Control.Comonad.Trans.Store
import Control.Category
import System.Directory
import Prelude hiding ((.), id)

import Control.Monad.Restricted

{-|
Monadic lenses.

Laws for pure monadic lenses:

 *  set-get: @(setL l b a >>= getL l)@ === @(setL l b a >> return b)@

 *  get-set: @(getL l a >>= \b -> setL l b a)@  ===  @(return a)@

 *  set-set: @(setL l b a >>= setL l b')@ ===  @(setL l b' a)@

For example, @fstLens@ and @(fstLens . fstLens)@ fulfil these laws.

Using lenses which do not fulfil the lens laws are safe,
but one should take extra care when doing program transformations
or reasoning about code with impure lenses.

The following law is a minimum, but some lenses (which do logging) do not fulfil this:

 *  get-no-effect: @(getL k a >> return ())@ === @(return ())@

TODO: List laws, document which laws hold for each lenses.
-}
newtype MLens m a b
    = MLens { runMLens :: a -> m (b, b -> m a) }
{-
The following representations would be also good for @(MLens m a b)@:

 *  @a -> m (Store b (m a))@

 *  @forall f . Functor f => (b -> m (f (m b))) -> a -> m (f (m a))@

 *  @(a -> m b, b -> a -> m a)@

The last representation has no efficient composition operation
(the set operation on composition of n lenses use O(n * n) get operations with the last representation).
-}

-- | Impure (but effect-free) lens constuctor
lensStore :: Monad m => (a -> (b, b -> a)) -> MLens m a b
lensStore f = MLens $ return . g . f where
    g (b, ba) = (b, return . ba)
{-
-- | Impure (but effect-free) lens constuctor, defined with @lensStore@.
lens :: Monad m => (a -> b) -> (b -> a -> a) -> MLens m a b
lens get set = lensStore $ \a -> (get a, flip set a)
-}
getL :: Monad m => MLens m a b -> a -> m b
getL k a = runMLens k a >>= return . fst

setL :: Monad m => MLens m a b -> b -> a -> m a
setL k b a = runMLens k a >>= ($ b) . snd

modL :: Monad m => MLens m b a -> (a -> a) -> b -> m b
modL k f b = do
    (x, h) <- runMLens k b
    h (f x)

instance Monad m => Category (MLens m) where
    id = MLens $ \a -> return (a, return)
    MLens r1 . MLens r2 = MLens $ \a -> do
        (g2, s2) <- r2 a
        (g1, s1) <- r1 g2
        return (g1, s1 >=> s2)

-- | Tensor product
--
-- could be defined as
--
-- @instance Monad m => Tensor (MLens m)@
--
-- @Tensor@ is defined in "Control.Category.Product" in the data-lens package.
(***) :: Monad m => MLens m a b -> MLens m c d -> MLens m (a, c) (b, d)
MLens r1 *** MLens r2 = MLens $ \(a1, a2) -> do
        (g1, s1) <- r1 a1
        (g2, s2) <- r2 a2
        return
            ( (g1, g2)
            , uncurry (liftM2 (,)) . (s1 Arrow.*** s2)
            )

infixr 3 ***

mapMLens :: (Monad m, Monad n) => Morph m n -> MLens m a b -> MLens n a b
mapMLens f (MLens r) = MLens $ \a -> do
    (x, s) <- f (r a)
    return (x, f . s)

joinML :: Monad m => (a -> m (MLens m a b)) -> MLens m a b
joinML r = MLens $ \x -> r x >>= ($ x) . runMLens

unitLens :: Lens a ()
unitLens = lens (const ()) (const id)

justLens :: a -> Lens (Maybe a) a
justLens a = lens (maybe a id) (const . Just)




{- | 
The abstract data type @Unit@ is isomorphic to @()@,
but by making it abstract we can prevent using the same reference
on both sides of lens composition, which would have surprising effects.
-}
data Unit = Unit deriving (Eq, Show)

{- |
Laws for pure references:

 *  @(readRef r >> return ())@ === @(return ())@

 *  @(readRef r >>= writeRef r)@ === @(return ())@

 *  @(writeRef r a >> readRef r)@ === @(return a)@

 *  @(writeRef r a >> writeRef r a')@ === @(writeRef r a')@

These laws are equivalent to the get-no-effect, set-get, get-set and set-set laws for monadic lenses.

Reference lenses can be composed with lenses.
For example, if

@r :: Ref m (a,b)@

then

@fstLens % r :: Ref m a@
-}
newtype Ref m a = Ref { unRef :: MLens m Unit a }

(%) :: Monad m => Lens a b -> Ref m a -> Ref m b
l % Ref k = Ref $ toMLens l
                . k

infixr 8 %

toMLens :: Monad m => Lens a b -> MLens m a b
toMLens l = lensStore (\a -> let (fb, b) = runStore $ runLens l a in (b, fb))

{-
(%) :: Monad m => MLens m a b -> Ref m a -> Ref m b
l % Ref k = Ref $ l . k

infixr 8 %
-}
unitRef :: Monad m => Ref m ()
unitRef = Ref $ toMLens unitLens

mapRef :: (Monad m, Monad n) => Morph m n -> Ref m a -> Ref n a
mapRef f (Ref r) = Ref $ mapMLens f r


runRef :: Monad m => Ref m a -> m (a, a -> m ())
runRef (Ref r) = liftM f $ runMLens r Unit where
    f (a, m) = (a, \a -> m a >> return ())

mkRef :: Monad m => m a -> (a -> m ()) -> Ref m a
mkRef r w = Ref $ MLens $ \unit -> do
    a <- r
    return $ (,) a $ \a -> do
        w a
        return unit

readRef :: Monad m => Ref m a -> m a
readRef = liftM fst . runRef

writeRef :: Monad m => Ref m a -> a -> m ()
writeRef r a = runRef r >>= ($ a) . snd

modRef :: Monad m => Ref m a -> (a -> a) -> m ()
k `modRef` f = runRef k >>= \(a, m) -> m $ f a

joinRef :: Monad m => m (Ref m a) -> Ref m a
joinRef m = Ref $ joinML $ const $ liftM unRef m

-- | Using @fileRef@ is safe if the file is not used concurrently.
fileRef :: FilePath -> IO (Ref IO String)
fileRef f = liftM (justLens "" %) $ fileRef_ f

-- | Note that if you write @Nothing@, the file is deleted.
fileRef_ :: FilePath -> IO (Ref IO (Maybe String))
fileRef_ f = return $ Ref $ MLens $ \unit -> do
    b <- doesFileExist f
    if b then do
            xs <- readFile f
            length xs `seq` return (Just xs, wr unit)
         else return (Nothing, wr unit)
 where
    wr unit mb = do
        maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f) mb
        return unit

logMLens :: Monad m => (a -> m ()) -> (a -> m ()) -> MLens m a a
logMLens getLog setLog = MLens $ \a -> getLog a >> return (a, \b -> setLog b >> return b)

{- |
@logConsoleLens@ logs elementary get and set operations.

Note that with the current representation of @MLens@, every set operation involves a get operation.
-}
logConsoleLens :: (Show a) => MLens IO a a
logConsoleLens = logMLens (putStrLn . ("Get: " ++) . show) (putStrLn . ("Set: " ++) . show)

logFile :: FilePath -> IO (String -> IO ())
logFile f = do
    b <- doesFileExist f
    when (not b) $ writeFile f ""
    return $ appendFile f



