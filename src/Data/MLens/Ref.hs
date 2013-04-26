{-# LANGUAGE RankNTypes #-}
module Data.MLens.Ref
    ( -- * Data type for references
      Ref (..)

    -- * Reference operations
    , (%)
    , mapRef
    , unitRef
    , modRef
    , joinRef
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
data Ref m a = Ref { readRef :: R m a, writeRef :: a -> m () }

mapRef :: Morph m n -> Ref m a -> Ref n a
mapRef f (Ref r w) = Ref (mapR f r) (f . w)


(%) :: Monad m => Lens a b -> Ref m a -> Ref m b
l % Ref r w = Ref r' w'
 where
    r' = liftM (L.getL l) r

    w' b = do
        a <- runR r
        w $ L.setL l b a

infixr 8 %


unitRef :: Monad m => Ref m ()
unitRef = Ref (return ()) (const $ return ())

modRef :: Monad m => Ref m a -> (a -> a) -> m ()
k `modRef` f = runR (readRef k) >>= writeRef k . f

joinRef :: Monad m => R m (Ref m a) -> Ref m a
joinRef m = Ref (m >>= readRef) (\a -> runR m >>= \r -> writeRef r a)


