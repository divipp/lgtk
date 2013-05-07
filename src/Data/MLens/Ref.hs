{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.MLens.Ref
    ( -- * Type classes for references
      Reference (..)
    , modRef
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
r `modRef` f = runR (readRef r) >>= writeRef r . f


