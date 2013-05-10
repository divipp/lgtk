{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( -- * Auxiliary definitions
      Morph
    , MMorph (..)
    ) where

type Morph m n = forall a . m a -> n a

class (Monad m, Monad (R m)) => MMorph m where

    type R m :: * -> *

    runR :: Morph (R m) m


