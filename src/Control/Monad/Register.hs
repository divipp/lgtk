{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Register
    ( Command (..)
    , MonadRegister (..)
    ) where

import Control.Monad.Restricted

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

class (Monad m, Monad (PureM m), Monad (EffectM m)) => MonadRegister m where

    type PureM m :: * -> *
    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

    toSend_ :: Eq b => Bool -> PureM m b -> (b -> m (m ())) -> m ()

    toReceive_ :: Eq a => (a -> PureM m ()) -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m ()




