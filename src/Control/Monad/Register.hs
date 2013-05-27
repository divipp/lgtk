{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( Command (..)
    , MonadRegister (..)
    ) where

import Control.Monad
import Control.Monad.Trans.Identity

import Control.Monad.Restricted

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

class (Monad m, Monad (EffectM m)) => MonadRegister m where

    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

    toSend_ :: Eq b => Bool -> EffectM m b -> (b -> m (m ())) -> m ()

    toReceive_ :: Eq a => (a -> EffectM m ()) -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m (Command -> EffectM m ())


instance MonadRegister m => MonadRegister (IdentityT m) where

    type EffectM (IdentityT m) = EffectM m

    liftEffectM m = IdentityT $ liftEffectM m

    toSend_ init m f = IdentityT $ toSend_ init m $ liftM runIdentityT . runIdentityT . f

    toReceive_ f g = IdentityT $ toReceive_ f g

