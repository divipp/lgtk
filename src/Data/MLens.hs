{-# LANGUAGE RankNTypes #-}
module Data.MLens
    ( -- * Monadic lenses data type
      MLens (..)

    -- * Side-effect free lenses
    , Lens
    , fromLens, toLens

    -- * Lens construction
    , lensStore
    , lens

    -- * Lens operations
    , getL, setL, modL

    -- * Lens transformations
    , (***)
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
import qualified Control.Arrow as Arrow
import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Prelude hiding ((.), id)

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

{-|
Side-effect free lenses.

The following representations would be also good for @(Lens a b)@:

 *  @forall m . Monad m => MLens m a b@
-}
type Lens a b
    = MLens Identity a b

fromLens :: Monad m => Lens a b -> MLens m a b
fromLens = mapMLens (return . runIdentity)

toLens :: (forall m . Monad m => MLens m a b) -> Lens a b
toLens k = k

-- | Impure (but effect-free) lens constuctor
lensStore :: Monad m => (a -> (b, b -> a)) -> MLens m a b
lensStore f = MLens $ return . g . f where
    g (b, ba) = (b, return . ba)

-- | Impure (but effect-free) lens constuctor, built on @lensStore@.
lens :: Monad m => (a -> b) -> (b -> a -> a) -> MLens m a b
lens get set = lensStore $ \a -> (get a, flip set a)

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

-- | It would be possible to define a @Monad@ instance for @(MLens m a)@ too, but monad laws would not hold.
joinLens :: Monad m => MLens m a (MLens m a b) -> MLens m a b
joinLens = joinML . getL

unitLens :: Monad m => MLens m a ()
unitLens = lens (const ()) (const id)

fstLens :: Monad m => MLens m (a,b) a
fstLens = lens fst $ \a (_,b) -> (a,b)

sndLens :: Monad m => MLens m (a,b) b
sndLens = lens snd $ \b (a,_) -> (a,b)

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
forkLens = lens (\a -> (a, a)) $ \(a1, a2) _ -> a1 `mappend` a2

justLens :: Monad m => a -> MLens m (Maybe a) a
justLens a = lens (maybe a id) (const . Just)

showLens :: (Monad m, Show a, Read a) => MLens m a String
showLens = lens show $ \s def -> maybe def fst $ listToMaybe $ reads s


type Morph m n = forall a . m a -> n a





