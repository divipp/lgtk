{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.MLens.Ref
    ( -- * Type class for references
      Reference (..)
    , modRef

    -- * Data type for references
    , Ref (..)
    , mapRef
    ) where

import Control.Monad.Identity
import Data.Lens.Common
import qualified Data.Lens.Common as L
import Control.Category
import Prelude hiding ((.), id)

import Control.Monad.Restricted

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
class Monad (RefMonad r) => Reference r where

    type RefMonad r :: * -> *

    readRef  :: r a -> R (RefMonad r) a
    writeRef :: r a -> a -> RefMonad r ()
    (%) :: Lens a b -> r a -> r b
    joinRef :: R (RefMonad r) (r a) -> r a
    unitRef :: r ()

infixr 8 %

modRef :: Reference r => r a -> (a -> a) -> RefMonad r ()
k `modRef` f = runR (readRef k) >>= writeRef k . f


data Ref m a = Ref { readRef_ :: R m a, writeRef_ :: a -> m () }

mapRef :: Morph m n -> Ref m a -> Ref n a
mapRef f (Ref r w) = Ref (mapR f r) (f . w)

instance Monad m => Reference (Ref m) where

    type RefMonad (Ref m) = m

    readRef = readRef_

    writeRef = writeRef_

    l % Ref r w = Ref r' w'
     where
        r' = liftM (L.getL l) r

        w' b = do
            a <- runR r
            w $ L.setL l b a

    unitRef = Ref (return ()) (const $ return ())

    joinRef m = Ref (m >>= readRef_) (\a -> runR m >>= \r -> writeRef_ r a)


