{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Register.Basic
    ( Register
    , evalRegister
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Prelude hiding ((.), id)

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef

data RegisterState n m k = RegisterState
    { actions :: k ()
    , sendEvent :: k () -> k ()
    , morph :: MorphD m k
    , morphN :: MorphD n k
    , morphK :: MorphD k m
    , newRef' :: forall a . a -> k (MorphD (State a) k)
    }

newtype MM a = MM { runMM :: a () }

instance Monad m => Monoid (MM m) where
    mempty = MM $ return ()
    MM a `mappend` MM b = MM $ a >> b

newtype Register n k m a = Register { runRegister :: ReaderT (RegisterState n m k) (WriterT (MM k, Command -> MM k) m) a }
    deriving (Functor, Monad, MonadIO)

instance (Monad k) => MonadTrans (Register n k) where
    lift = Register . lift . lift

instance (ExtRef m, n ~ WriteRef m, Monad k) => ExtRef (Register n k m) where

    type Ref (Register n k m) = Ref m

    liftWriteRef = Register . liftWriteRef

    extRef r k a = Register $ extRef r k a

liftIO' :: (Monad k, Monad m) => k a -> Register n k m a
liftIO' m = do
    rr <- Register $ ask
    lift $ runMorphD (morphK rr) m

instance (Monad m, HasReadPart n, Monad k) => MonadRegister (Register n k m) where

    type PureM (Register n k m) = n
    type EffectM (Register n k m) = k

    liftEffectM = liftIO'

    toReceive r int = do
        rr <- Register ask
        unreg <- liftEffectM $ int $ \a -> sendEvent rr $ do
            runMorphD (morphN rr) $ r a
            actions rr
        Register $ tell $ t2 unreg
        return ()

    toSend bb rb fb = do
        rr <- Register ask
        memoref <- liftIO' $ newRef' rr (const $ return (), const $ return (), [])  -- unreg action, memo table, first item is the newest
        Register $ tell $ t1 $ do
            b <- runMorphD (morphN rr) $ runR rb
            let doit c (s1, ureg1) = do 
                    (s2_, ureg2_) <- runMorphD (morph rr) $ execWriterT $ runReaderT (runRegister c) rr
                    let s2 = runMM s2_
                        ureg2 = runMM . ureg2_
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
                        (c, s1_) <- runMorphD (morph rr) $ runWriterT $ runReaderT (runRegister $ fb b) rr
                        let s1 = (runMM $ fst s1_, runMM . snd s1_)
                        doit c s1

t1 m = (MM m, mempty)
t2 m = (mempty, MM . m)

-- | evaluation with postponed actions
evalRegister :: (Monad n, Monad m, Monad k)
    => (forall a . a -> k (MorphD (State a) k))
    -> Morph n k
    -> Morph k m
    -> Morph m k
    -> ((k () -> k ()) -> Register n k m a)
    -> (k () -> k ())
    -> m a
evalRegister newRef' morphN liftIO morph f ch = do
    post <- liftIO $ newRef' $ return ()
    let (Register m) = f $ runMorphD post . modify . flip (>>)
    vx <- liftIO $ newRef' $ error "evalRegister"
    (a, reg) <- runWriterT $ runReaderT m $ RegisterState (join (runMorphD vx get) >> join (runMorphD post $ state $ \m -> (m, return ()))) ch (MorphD morph) (MorphD morphN) (MorphD liftIO) newRef'
    liftIO $ runMorphD vx $ put $ runMM $ fst reg
    liftIO $ runMM $ fst reg        -- needed?
    return a

