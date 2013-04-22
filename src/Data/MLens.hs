{-# LANGUAGE RankNTypes #-}
module Data.MLens
    ( -- * Monadic lenses data type
      MLens (..)

    -- * Lens construction
    , lensStore
    , lens

    -- * Lens operations
    , getL, setL, modL

    -- * Lens transformations
    , (***)
    , mapMLens
    , joinML

    -- * Pure lenses
    , unitLens

    -- * Impure lenses
    , justLens

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

-- | Impure (but effect-free) lens constuctor
lensStore :: Monad m => (a -> (b, b -> a)) -> MLens m a b
lensStore f = MLens $ return . g . f where
    g (b, ba) = (b, return . ba)

-- | Impure (but effect-free) lens constuctor, defined with @lensStore@.
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

unitLens :: Monad m => MLens m a ()
unitLens = lens (const ()) (const id)

justLens :: Monad m => a -> MLens m (Maybe a) a
justLens a = lens (maybe a id) (const . Just)


type Morph m n = forall a . m a -> n a





