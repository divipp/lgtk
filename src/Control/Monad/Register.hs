{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( -- * Type synonymes and basic functions
      Send
    , voidSend
    , constSend
    , Receive
    , Command (..)

    -- * Register monad
    , MonadRegister (..)

    -- * Derived
    , rEffect
    ) where

import Control.Monad.Restricted

type Send m a = (a -> EffectM m ()) -> m ()

{- not possible, @Send m@ is not an applicative functor
lift2 :: (a -> b -> c) -> Send m a -> Send m b -> Send m c
lift2 f ga gb h = ga $ \a -> gb $ \b -> h $ f a b
-}

voidSend :: Monad m => Send m a
voidSend _ = return ()

constSend :: (MonadRegister m) => a -> Send m a 
constSend a f = liftEffectM $ f a

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

type Receive m a = ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m ()

class (Monad m, Monad (PureM m), Monad (EffectM m)) => MonadRegister m where

    type PureM m :: * -> *
    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

    toSend :: Eq b => Bool -> R (PureM m) b -> (b -> m (m ())) -> m ()

    toReceive :: Eq a => (a -> PureM m ()) -> Receive m a

rEffect :: (MonadRegister m, Eq a) => R (PureM m) a -> Send m a
rEffect r f = toSend False r $ return . liftEffectM . f





