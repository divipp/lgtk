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
    , addCEffect
    , constEffect
    , rEffect
    ) where

import Control.Monad.Restricted

type Receiver m a = (a -> Inn m ()) -> m ()

mapReceiver :: (a -> b) -> Receiver m a -> Receiver m b
mapReceiver f g h = g $ \a -> h $ f a

{- not possible, @Receiver m@ is not an applicative functor
lift2 :: (a -> b -> c) -> Receiver m a -> Receiver m b -> Receiver m c
lift2 f ga gb h = ga $ \a -> gb $ \b -> h $ f a b
-}

voidReceiver :: Monad m => Receiver m a
voidReceiver _ = return ()

type Sender m a = ((a -> Inn m ()) -> Inn m ()) -> m ()

data IC m a = forall b . Eq b => IC (R (PureM m) b) (b -> C m a)

class (Monad m, Monad (PureM m), Monad (Inn m)) => MonadRegister m where

    type PureM m :: * -> *
    type Inn m :: * -> *

    liftInn :: Morph (Inn m) m

    addICEffect :: Bool -> IC m a -> Receiver m a

    addWEffect :: Eq a => (a -> PureM m ()) -> Sender m a

constEffect :: (MonadRegister m) => a -> Receiver m a 
constEffect a f = liftInn $ f a

addCEffect :: (MonadRegister m, Eq a) => R (PureM m) a -> Receiver m a
addCEffect r = addICEffect False (IC r return)

rEffect :: (MonadRegister m, Eq a) => R (PureM m) a -> Receiver m a
rEffect = addCEffect

