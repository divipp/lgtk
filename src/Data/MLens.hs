{-# LANGUAGE RankNTypes #-}
module Data.MLens
    ( -- * Monadic lenses data type
      MLens (MLens)

    -- * Side-effect free lenses
    , Lens
    , fromLens, toLens

    -- * Lens construction
    , lens

    -- * Lens operations
    , getL, setL, modL

    -- * Lens transformations
    , mapMLens
    , joinML, joinLens

    -- * Pure lenses
    , unitLens
    , fstLens, sndLens
    , maybeLens
    , listLens
    , ithLens

    -- * Impure lenses
    , forkLens
    , justLens
    , showLens

    -- * Auxiliary definitions
    , Morph
    ) where

import Data.Monoid
import Control.Category
import Control.Category.Product
import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Prelude hiding ((.), id)

{-|
Monadic lenses.

The following representations would be also good for @(MLens m a b)@:

 *  @a -> m (Store b (m a))@

 *  @forall f . Functor f => (b -> m (f (m b))) -> a -> m (f (m a))@

 *  @(a -> m b, b -> a -> m a)@

The last representation has no efficient composition operation
(the set operation on composition of n lenses use O(n * n) get operations with the last representation).

Using lenses which do not fulfil the lens laws are safe,
but one should take extra care when doing program transformations
or reasoning about code with impure lenses.

The following law is a minimum, but some lenses (which do logging) do not fulfil this:

 *  @getL@ has no side effect, i.e. @(getL k a >> return ())@  can be replaced by  @(return ())@

TODO: List laws, document which laws hold for each lenses.
-}
newtype MLens m a b
    = MLens (a -> m (b, b -> m a))

{-|
Side-effect free lenses.

The following representations would be also good for @(Lens a b)@:

 *  @forall m . Monad m => MLens m a b@
-}
type Lens a b
    = MLens Identity a b

fromLens :: Monad m => Lens a b -> MLens m a b
fromLens (MLens f) = MLens $ \x -> do
    let (a, b) = runIdentity $ f x
    return (a, \y -> return $ runIdentity $ b y)

toLens :: (forall m . Monad m => MLens m a b) -> Lens a b
toLens k = k

lens :: Monad m => (a -> b) -> (b -> a -> a) -> MLens m a b
lens get set = MLens $ \a -> return (get a, return . flip set a)

getL :: Monad m => MLens m a b -> a -> m b
getL (MLens f) a = f a >>= return . fst

setL :: Monad m => MLens m a b -> b -> a -> m a
setL (MLens f) b a = f a >>= ($ b) . snd

modL :: Monad m => MLens m b a -> (a -> a) -> b -> m b
modL (MLens g) f b = do
    (x, h) <- g b
    h (f x)

instance Monad m => Category (MLens m) where
    id = MLens $ \a -> return (a, return)
    MLens r1 . MLens r2 = MLens $ \a -> do
        (g2, s2) <- r2 a
        (g1, s1) <- r1 g2
        return (g1, s1 >=> s2)

instance Monad m => Tensor (MLens m) where
    MLens r1 *** MLens r2 = MLens $ \(a1, a2) -> do
        (g1, s1) <- r1 a1
        (g2, s2) <- r2 a2
        return
            ( (g1, g2)
            , uncurry (liftM2 (,)) . (s1 *** s2)
            )

mapMLens :: (Monad m, Monad n) => Morph m n -> MLens m a b -> MLens n a b
mapMLens f (MLens r) = MLens $ \a -> do
    (x, s) <- f (r a)
    return (x, f . s)

joinML :: Monad m => (a -> m (MLens m a b)) -> MLens m a b
joinML r = MLens $ \x -> do
    MLens q <- r x
    q x

-- | It would be possible to define a @Monad@ instance for @(MLens m a)@ too, but monad laws would not hold.
joinLens :: Monad m => MLens m a (MLens m a b) -> MLens m a b
joinLens = joinML . getL

unitLens :: Monad m => MLens m a ()
unitLens = lens (const ()) (const id)

fstLens :: Monad m => MLens m (a,b) a
fstLens = MLens $ \(a,b) -> return (a, \a' -> return (a', b))

sndLens :: Monad m => MLens m (a,b) b
sndLens = MLens $ \(a,b) -> return (b, \b' -> return (a, b'))

maybeLens :: Monad m => MLens m (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\x (_,a) -> maybe (False, a) (\a' -> (True, a')) x)

listLens :: Monad m => MLens m (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set [] (_, x) = (False, x)
    set (l: r) _ = (True, (l, r))

-- | @ithLens@ is pure only with proper preconditions.
ithLens :: Monad m => Int -> MLens m [a] a
ithLens i = lens (!!i) $ \x xs -> take i xs ++ x : drop (i+1) xs

forkLens :: (Monoid a, Monad m) => MLens m a (a, a)
forkLens = MLens $ \a ->
    return ((a, a), \(a1, a2) -> return $ a1 `mappend` a2)

justLens :: Monad m => a -> MLens m (Maybe a) a
justLens a = lens (maybe a id) (const . Just)

showLens :: (Monad m, Show a, Read a) => MLens m a String
showLens = lens show $ \s def -> maybe def fst $ listToMaybe $ reads s


type Morph m n = forall a . m a -> n a





