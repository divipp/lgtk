{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( R (..), mapR
    , C (..), mapC
    , rToC
    ) where

import Data.MLens (Morph)

newtype R m a = R { runR :: m a } deriving (Monad)

mapR :: Morph m n -> R m a -> R n a
mapR f (R x) = R (f x)

newtype C m a = C { runC :: m a } deriving (Monad)

mapC :: Morph m n -> C m a -> C n a
mapC f (C x) = C (f x)

rToC :: R m a -> C m a
rToC (R m) = C m

