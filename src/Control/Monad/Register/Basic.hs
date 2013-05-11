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
import Control.Concurrent
import Data.IORef
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

newtype Register n m k a = Register { runRegister :: ReaderT (RegisterState n m k) (WriterT (MM k, Command -> MM k) m) a }
    deriving (Functor, Monad, MonadIO)

instance (ExtRef m, n ~ WriteRef m, Monad k) => ExtRef (Register n m k) where

    type Ref (Register n m k) = Ref m

    liftWriteRef = Register . liftWriteRef

    extRef r k a = Register $ extRef r k a

liftIO' :: (Monad k, Monad m) => k a -> Register n m k a
liftIO' m = do
    rr <- Register $ ask
    Register $ lift $ lift $ runMorphD (morphK rr) m

instance (MonadIO m, MMorph n, Monad k) => MonadRegister (Register n m k) where

    type PureM (Register n m k) = n
    type EffectM (Register n m k) = k

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
            join $ runMorphD memoref $ state $ \memo -> flip (,) memo $ case memo of
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
evalRegister :: (Monad n, MonadIO m) => Morph n IO -> Morph m IO -> ((IO () -> IO ()) -> Register n m IO a) -> m a
evalRegister morphN morph f = do
    post <- liftIO $ newIORef $ return ()
    let (Register m) = f $ \io -> atomicModifyIORef' post $ \m -> (m >> io, ())
    vx <- liftIO $ newIORef $ return ()
    ch <- liftIO newChan
    let g x = do
            vx <- newIORef x
            return $ MorphD $ \m -> atomicModifyIORef' vx $ swap . runState m
        swap (a, b) = (b, a)
    (a, reg) <- runWriterT $ runReaderT m $ RegisterState (join (readIORef vx) >> join (atomicModifyIORef' post (\m -> (return (), m)))) (writeChan ch) (MorphD morph) (MorphD morphN) (MorphD liftIO) g
    liftIO $ writeIORef vx $ runMM $ fst reg
    _ <- liftIO $ forkIO $ forever $ join $ readChan ch
    liftIO $ runMM $ fst reg        -- needed?
    return a

