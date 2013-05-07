{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( MonadRegister (..)
    , IC (..)
    , Receiver
    , mapReceiver
    , voidReceiver
    , Sender
    , addCEffect
    , constEffect
    , rEffect
    ) where

import Control.Monad.Restricted

type Receiver m a = (a -> Inn m ()) -> m ()

mapReceiver :: (a -> b) -> Receiver m a -> Receiver m b
mapReceiver f g h = g $ \a -> h $ f a

voidReceiver :: Monad m => Receiver m a
voidReceiver _ = return ()

type Sender m a = ((a -> Inn m ()) -> Inn m ()) -> m ()

data IC m a = forall b . Eq b => IC (R (Inner' m) b) (b -> C m a)

class (Monad m, Monad (Inner' m), Monad (Inn m)) => MonadRegister m where

    type Inner' m :: * -> *
    type Inn m :: * -> *

    liftInn :: Morph (Inn m) m

    addICEffect :: Bool -> IC m a -> Receiver m a

    addWEffect :: Eq a => (a -> Inner' m ()) -> Sender m a

constEffect :: (MonadRegister m) => a -> Receiver m a 
constEffect a f = liftInn $ f a

addCEffect :: (MonadRegister m, Eq a) => R (Inner' m) a -> Receiver m a
addCEffect r = addICEffect False (IC r return)

rEffect :: (MonadRegister m, Eq a) => R (Inner' m) a -> Receiver m a
rEffect = addCEffect

