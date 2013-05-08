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

instance (ExtRef m, n ~ Inner m) => ExtRef (Register n m) where

    type Ref (Register n m) = Ref m

    liftInner = Register . liftInner

    newRef = mapC Register . newRef

    extRef r k a = mapC Register $ extRef r k a

instance (MonadIO m, Monad n) => MonadRegister (Register n m) where

    type PureM (Register n m) = n
    type EffectM (Register n m) = IO

    liftEffectM = Register . liftIO

    toReceive r int = do
        rr <- Register ask
        liftEffectM $ int $ \a -> sendEvent rr $ do
            unlift (morphN rr) $ r a
            actions rr

    toSend bb (IC rb fb) act = do
        rr <- Register ask
        memoref <- liftIO $ newIORef []  -- memo table, first item is the newest
        Register $ tell $ do
            b <- unlift (morphN rr) $ runR rb
            join $ atomicModifyIORef' memoref $ \memo -> case memo of
                ((b', (_, s)): _) | b' == b -> (memo, s)
                _ -> case partition ((== b) . fst) memo of
                    (x@(_, (c, s)): _, rem) -> (x: rem, act c >> s)
                    _ -> (,) memo $ do
                        (c, s) <- unlift (morph rr) $ runWriterT $ runReaderT (unRegister $ runC $ fb b) rr
                        when bb $ atomicModifyIORef' memoref $ \memo -> ((b, (c, s)) : filter ((/= b) . fst) memo, ())
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

