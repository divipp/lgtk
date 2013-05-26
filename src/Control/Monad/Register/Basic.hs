{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Register.Basic
    ( evalRegister
    , evalRegisterBasic
    ) where

import Control.Monad
import Control.Monad.RWS
import Data.List
import Prelude hiding ((.), id)

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef

type Register m
    = RWST (m () -> m ()) (MonadMonoid m, Command -> MonadMonoid m) () m

instance NewRef m => MonadRegister (Register m) where

    type EffectM (Register m) = m

    liftEffectM = lift

    toReceive_ r int = do
        rr <- ask
        unreg <- lift $ int $ rr . r
        tell $ t2 unreg

    toSend_ rb fb = do
        rr <- ask
        memoref <- lift $ newRef' []  -- memo table, first item is the newest
        tell $ t1 $ do
            b <- rb
            (s1, s2) <- join $ runMorphD memoref $ gets $ \memo -> case memo of
                ((b', (_, s1, s2, _, _)): _) | b' == b -> return (s1, s2)
                _ -> do
                    case memo of
                        ((_, (_, _, _, ureg1, ureg2)): _) ->
                            runMonadMonoid $ ureg1 Block `mappend` ureg2 Kill
                        _ -> return ()
                    (c, (), (s1, ureg1)) <- case filter ((== b) . fst) memo of
                        ((_, (c, s1, _, ureg1, _)): _) -> do
                            runMonadMonoid $ ureg1 Unblock
                            return (c, (), (s1, ureg1))
                        _ -> runRWST (fb b) rr ()
                    ((), (s2, ureg2)) <- execRWST c rr ()
                    runMorphD memoref $ state $ \memo ->
                        (,) () $ (b, (c, s1, s2, ureg1, ureg2)) : filter ((/= b) . fst) memo
                    return (s1, s2)
            runMonadMonoid $ s1 `mappend` s2

t1 m = (MonadMonoid m, mempty)
t2 m = (mempty, MonadMonoid . m)

evalRegister :: forall k a . (NewRef k, ExtRef k, MonadIO k, SafeIO k)
    => (forall t . (MonadTrans t, MonadRegister (t k), MonadIO (t k)
       , ExtRef (t k), Ref (t k) ~ Ref k, EffectM (t k) ~ k, SafeIO (t k)) => t k a)
    -> (k () -> k ())
    -> k a
evalRegister m = evalRegister_ m

evalRegisterBasic
    :: forall k a . NewRef k
    => (forall t . (MonadTrans t, MonadRegister (t k)) => t k a)
    -> (k () -> k ())
    -> k a
evalRegisterBasic m = evalRegister_ m

evalRegister_
    :: NewRef k
    => (Register k a)
    -> (k () -> k ())
    -> k a
evalRegister_ m ch = do
    vx <- newRef' $ error "evalRegister"
    (a, (), reg) <- runRWST m (ch . (>> join (runMorphD vx get))) ()
    runMorphD vx $ put $ runMonadMonoid $ fst reg
    return a



