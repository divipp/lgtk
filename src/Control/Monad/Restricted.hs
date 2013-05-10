{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( R, runR, mapR
    , unsafeR

    -- * Auxiliary definitions
    , Morph
    ) where

newtype R m a = R { runR :: m a } deriving (Functor, Monad)

unsafeR :: m a -> R m a
unsafeR = R

mapR :: Morph m n -> R m a -> R n a
mapR f (R x) = R (f x)

type Morph m n = forall a . m a -> n a

