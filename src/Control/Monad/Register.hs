{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( -- * Type synonymes and basic functions
      Send
    , mapSend
    , voidSend
    , constSend
    , Receive
    , mapReceive
    , voidReceive

    -- * Register monad
    , MonadRegister (..)
    , liftEffectMC

    -- * Derived
    , rEffect
    , asyncToSend
    ) where

import Control.Monad.Restricted

type Send m a = (a -> EffectM m ()) -> m ()

mapSend :: (a -> b) -> Send m a -> Send m b
mapSend f = (. (. f))

{- not possible, @Send m@ is not an applicative functor
lift2 :: (a -> b -> c) -> Send m a -> Send m b -> Send m c
lift2 f ga gb h = ga $ \a -> gb $ \b -> h $ f a b
-}

voidSend :: Monad m => Send m a
voidSend _ = return ()

constSend :: (MonadRegister m) => a -> Send m a 
constSend a f = liftEffectM $ f a

type Receive m a = ((a -> EffectM m ()) -> EffectM m ()) -> m ()

mapReceive :: (b -> a) -> Receive m a -> Receive m b
mapReceive f = (. (. (. f)))

voidReceive :: Monad m => Receive m a
voidReceive _ = return ()

class (Monad m, Monad (PureM m), Monad (EffectM m)) => MonadRegister m where

    type PureM m :: * -> *
    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

    toSend :: Eq b => Bool -> R (PureM m) b -> (b -> C m a) -> Send m a

    toReceive :: Eq a => (a -> PureM m ()) -> Receive m a

rEffect :: (MonadRegister m, Eq a) => R (PureM m) a -> Send m a
rEffect r = toSend False r return

-- | TODO
liftEffectMC :: MonadRegister m => Morph (EffectM m) (C m)
liftEffectMC = unsafeC . liftEffectM

asyncToSend
    :: (Eq b, MonadRegister m)
    => Bool
    -> R (PureM m) b
    -> (b -> (t -> EffectM m ()) -> C m ())
    -> Send m t
asyncToSend b x y re = toSend b x (\b -> y b re) return







