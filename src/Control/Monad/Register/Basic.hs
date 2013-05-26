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

    toSend_ bb rb fb = do
        rr <- ask
        memoref <- lift $ newRef' (const $ return (), const $ return (), [])  -- unreg action, memo table, first item is the newest
        tell $ t1 $ do
            b <- rb
            let doit c (s1, ureg1) = do 
                    ((), (s2_, ureg2_)) <- execRWST c rr ()
                    let s2 = runMonadMonoid s2_
                        ureg2 = runMonadMonoid . ureg2_
                    runMorphD memoref $ state $ \(_, _, memo) -> (,) () (ureg1, ureg2, (b, (c, s1, s2, ureg1, ureg2)) : if bb then filter ((/= b) . fst) memo else [])
                    s1 >> s2
            join $ runMorphD memoref $ gets $ \memo -> case memo of
                (_, _, ((b', (_, s1, s2, _, _)): _)) | b' == b -> s1 >> s2
                (ur1, ur2, memo) -> do
                  ur1 $ if bb then Block else Kill
                  ur2 Kill
                  case (bb, filter ((== b) . fst) memo) of
                    (True, (_, (c, s1, _, ureg1, ureg2)): _) -> ureg1 Unblock >> doit c (s1, ureg1)
                    _ -> do
                        (c, (), s1_) <- runRWST (fb b) rr ()
                        let s1 = (runMonadMonoid $ fst s1_, runMonadMonoid . snd s1_)
                        doit c s1

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
    runMonadMonoid $ fst reg        -- needed?
    return a



