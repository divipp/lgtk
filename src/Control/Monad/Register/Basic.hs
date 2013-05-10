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
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent
import Data.IORef
import Data.List
import Prelude hiding ((.), id)

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef

data MorphD m n = MorphD { unlift :: Morph m n }

data RegisterState n m = RegisterState
    { actions :: IO ()
    , sendEvent :: IO () -> IO ()
    , morph :: MorphD m IO
    , morphN :: MorphD n IO
    }

instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)

newtype Register n m a = Register { unRegister :: ReaderT (RegisterState n m) (WriterT (IO ()) m) a }
    deriving (Functor, Monad, MonadIO)

instance (ExtRef m, n ~ WriteRef m) => ExtRef (Register n m) where

    type Ref (Register n m) = Ref m

    liftWriteRef = Register . liftWriteRef

    extRef r k a = Register $ extRef r k a

instance (MonadIO m, MMorph n) => MonadRegister (Register n m) where

    type PureM (Register n m) = n
    type EffectM (Register n m) = IO

    liftEffectM = Register . liftIO

    toReceive r int = do
        rr <- Register ask
        liftEffectM $ int $ \a -> sendEvent rr $ do
            unlift (morphN rr) $ r a
            actions rr

    toSend bb rb fb act = do
        rr <- Register ask
        memoref <- liftIO $ newIORef []  -- memo table, first item is the newest
        Register $ tell $ do
            b <- unlift (morphN rr) $ runR rb
            join $ atomicModifyIORef' memoref $ \memo -> case memo of
                ((b', (_, s)): _) | b' == b -> (memo, s)
                _ -> case (bb, partition ((== b) . fst) memo) of
                    (True, (x@(_, (c, s)): _, rem)) -> (x: rem, act c >> s)
                    _ -> (,) memo $ do
                        (c, s) <- unlift (morph rr) $ runWriterT $ runReaderT (unRegister $ fb b) rr
                        atomicModifyIORef' memoref $ \memo -> ((b, (c, s)) : if bb then filter ((/= b) . fst) memo else [], ())
                        act c >> s

-- | evaluation with postponed actions
evalRegister :: (Monad n, MonadIO m) => Morph n IO -> Morph m IO -> ((IO () -> IO ()) -> Register n m a) -> m a
evalRegister morphN morph f = do
    post <- liftIO $ newIORef $ return ()
    let (Register m) = f $ \io -> atomicModifyIORef' post $ \m -> (m >> io, ())
    vx <- liftIO $ newIORef $ return ()
    ch <- liftIO newChan
    (a, reg) <- runWriterT $ runReaderT m $ RegisterState (join (readIORef vx) >> join (atomicModifyIORef' post (\m -> (return (), m)))) (writeChan ch) (MorphD morph) (MorphD morphN)
    liftIO $ writeIORef vx reg
    _ <- liftIO $ forkIO $ forever $ join $ readChan ch
    liftIO reg
    return a

