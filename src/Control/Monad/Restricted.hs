{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( -- * Auxiliary definitions
      Morph
    , MorphD (..)
    , MMorph (..)
    ) where

type Morph m n = forall a . m a -> n a

newtype MorphD m n = MorphD { runMorphD :: Morph m n }

class (Monad m, Monad (R m)) => MMorph m where

    type R m :: * -> *

    runR :: Morph (R m) m


