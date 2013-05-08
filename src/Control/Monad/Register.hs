{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( Send
    , mapSend
    , voidSend
    , Receive
    , IC (..)
    , MonadRegister (..)
    , constSend
    , rEffect
    ) where

import Control.Monad.Restricted

type Send m a = (a -> EffectM m ()) -> m ()

mapSend :: (a -> b) -> Send m a -> Send m b
mapSend f g h = g $ \a -> h $ f a

{- not possible, @Send m@ is not an applicative functor
lift2 :: (a -> b -> c) -> Send m a -> Send m b -> Send m c
lift2 f ga gb h = ga $ \a -> gb $ \b -> h $ f a b
-}

voidSend :: Monad m => Send m a
voidSend _ = return ()

type Receive m a = ((a -> EffectM m ()) -> EffectM m ()) -> m ()

data IC m a = forall b . Eq b => IC (R (PureM m) b) (b -> C m a)

class (Monad m, Monad (PureM m), Monad (EffectM m)) => MonadRegister m where

    type PureM m :: * -> *
    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

    toSend :: Bool -> IC m a -> Send m a

    addWEffect :: Eq a => (a -> PureM m ()) -> Receive m a

constSend :: (MonadRegister m) => a -> Send m a 
constSend a f = liftEffectM $ f a

rEffect :: (MonadRegister m, Eq a) => R (PureM m) a -> Send m a
rEffect r = toSend False (IC r return)

