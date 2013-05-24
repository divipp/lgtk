{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( -- * Auxiliary definitions
      Morph
    , MorphD (..)
    , HasReadPart (..)
    ) where

type Morph m n = forall a . m a -> n a

newtype MorphD m n = MorphD { runMorphD :: Morph m n }

class (Monad m, Monad (ReadPart m)) => HasReadPart m where

    type ReadPart m :: * -> *

    runR :: Morph (ReadPart m) m


