{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( R, runR, mapR
    , C, runC, mapC
    , rToC
    , unsafeC, unsafeR, unsafeCToR

    -- * Auxiliary definitions
    , Morph
    ) where

newtype R m a = R { runR :: m a } deriving (Functor, Monad)

unsafeR :: m a -> R m a
unsafeR = R

mapR :: Morph m n -> R m a -> R n a
mapR f (R x) = R (f x)

newtype C m a = C { runC :: m a } deriving (Functor, Monad)

unsafeC :: m a -> C m a
unsafeC = C

mapC :: Morph m n -> C m a -> C n a
mapC f (C x) = C (f x)

rToC :: R m a -> C m a
rToC (R m) = C m

unsafeCToR :: C m a -> R m a
unsafeCToR = R . runC

type Morph m n = forall a . m a -> n a

