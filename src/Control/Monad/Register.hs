{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( Receiver
    , mapReceiver
    , voidReceiver
    , Sender
    , IC (..)
    , MonadRegister (..)
    , constEffect
    , rEffect
    ) where

import Control.Monad.Restricted

type Receiver m a = (a -> EffectM m ()) -> m ()

mapReceiver :: (a -> b) -> Receiver m a -> Receiver m b
mapReceiver f g h = g $ \a -> h $ f a

{- not possible, @Receiver m@ is not an applicative functor
lift2 :: (a -> b -> c) -> Receiver m a -> Receiver m b -> Receiver m c
lift2 f ga gb h = ga $ \a -> gb $ \b -> h $ f a b
-}

voidReceiver :: Monad m => Receiver m a
voidReceiver _ = return ()

type Sender m a = ((a -> EffectM m ()) -> EffectM m ()) -> m ()

data IC m a = forall b . Eq b => IC (R (PureM m) b) (b -> C m a)

class (Monad m, Monad (PureM m), Monad (EffectM m)) => MonadRegister m where

    type PureM m :: * -> *
    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

    addICEffect :: Bool -> IC m a -> Receiver m a

    addWEffect :: Eq a => (a -> PureM m ()) -> Sender m a

constEffect :: (MonadRegister m) => a -> Receiver m a 
constEffect a f = liftEffectM $ f a

rEffect :: (MonadRegister m, Eq a) => R (PureM m) a -> Receiver m a
rEffect r = addICEffect False (IC r return)

