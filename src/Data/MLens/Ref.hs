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
--    , getL, setL, modL

    -- * Pure lenses
    , unitLens
    ) where

import qualified Control.Arrow as Arrow
import Control.Monad.Identity
import Data.Maybe
import Data.Lens.Common hiding (getL, setL, modL)
import qualified Data.Lens.Common as L
import Control.Comonad.Trans.Store
import Control.Category
import System.Directory
import Prelude hiding ((.), id)

import Control.Monad.Restricted

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
data Ref m a = Ref (m a) (a -> m ())

runRef :: Monad m => Ref m a -> m (a, a -> m ())
runRef (Ref r w) = r >>= \a -> return (a, w)

mkRef :: Monad m => m a -> (a -> m ()) -> Ref m a
mkRef = Ref

mapRef :: (Monad m, Monad n) => Morph m n -> Ref m a -> Ref n a
mapRef f r = mkRef (f $ liftM fst $ runRef r) (\b -> f $ join $ liftM (($ b) . snd) $ runRef r)


(%) :: Monad m => Lens a b -> Ref m a -> Ref m b
l % k = mkRef r w
 where
    r = liftM (L.getL l . fst) $ runRef k

    w b = do
        a <- liftM fst $ runRef k
        join $ liftM (($ L.setL l b a) . snd) $ runRef k

infixr 8 %


unitRef :: Monad m => Ref m ()
unitRef = mkRef (return ()) (const $ return ())

readRef :: Monad m => Ref m a -> m a
readRef = liftM fst . runRef

writeRef :: Monad m => Ref m a -> a -> m ()
writeRef r a = runRef r >>= ($ a) . snd

modRef :: Monad m => Ref m a -> (a -> a) -> m ()
k `modRef` f = runRef k >>= \(a, m) -> m $ f a

joinRef :: Monad m => m (Ref m a) -> Ref m a
joinRef m = mkRef (liftM fst $ m >>= runRef) (\a -> join $ liftM (($ a) . snd) $ m >>= runRef)


-- | Using @fileRef@ is safe if the file is not used concurrently.
fileRef :: FilePath -> IO (Ref IO String)
fileRef f = liftM (justLens "" %) $ fileRef_ f

-- | Note that if you write @Nothing@, the file is deleted.
fileRef_ :: FilePath -> IO (Ref IO (Maybe String))
fileRef_ f = return $ mkRef r w
 where
    r = do
        b <- doesFileExist f
        if b then do
            xs <- readFile f
            length xs `seq` return (Just xs)
         else return Nothing

    w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)



