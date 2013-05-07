{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Register.Basic
    ( EE
    , evalEE
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

data EEState n m = EEState
    { actions :: IO ()
    , sendEvent :: IO () -> IO ()
    , morph :: MorphD m IO
    , morphN :: MorphD n IO
    }

instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)

newtype EE n m a = EE { unEE :: ReaderT (EEState n m) (WriterT (IO ()) m) a }
    deriving (Functor, Monad, MonadIO)

instance (ExtRef m, n ~ Inner m) => ExtRef (EE n m) where

    type Ref (EE n m) = Ref m

    liftInner = EE . liftInner

    newRef = mapC EE . newRef

    extRef r k a = mapC EE $ extRef r k a

instance (MonadIO m, Monad n) => MonadRegister (EE n m) where

    type Inner' (EE n m) = n
    type Inn (EE n m) = IO

    liftInn = EE . liftIO

    addWEffect r int = do
        rr <- EE ask
        liftInn $ int $ \a -> sendEvent rr $ do
            unlift (morphN rr) $ r a
            actions rr

    addICEffect bb (IC rb fb) act = do
        rr <- EE ask
        memoref <- liftIO $ newIORef []  -- memo table, first item is the newest
        EE $ tell $ do
            b <- unlift (morphN rr) $ runR rb
            join $ atomicModifyIORef' memoref $ \memo -> case memo of
                ((b', (_, s)): _) | b' == b -> (memo, s)
                _ -> case partition ((== b) . fst) memo of
                    (x@(_, (c, s)): _, rem) -> (x: rem, act c >> s)
                    _ -> (,) memo $ do
                        (c, s) <- unlift (morph rr) $ runWriterT $ runReaderT (unEE $ runC $ fb b) rr
                        when bb $ atomicModifyIORef' memoref $ \memo -> ((b, (c, s)) : filter ((/= b) . fst) memo, ())
                        act c >> s

-- | evaluation with postponed actions
evalEE :: (Monad n, MonadIO m) => Morph n IO -> Morph m IO -> ((IO () -> IO ()) -> EE n m a) -> m a
evalEE morphN morph f = do
    post <- liftIO $ newIORef $ return ()
    let (EE m) = f $ \io -> atomicModifyIORef' post $ \m -> (m >> io, ())
    vx <- liftIO $ newIORef $ return ()
    ch <- liftIO newChan
    (a, reg) <- runWriterT $ runReaderT m $ EEState (join (readIORef vx) >> join (atomicModifyIORef' post (\m -> (return (), m)))) (writeChan ch) (MorphD morph) (MorphD morphN)
    liftIO $ writeIORef vx reg
    _ <- liftIO $ forkIO $ forever $ join $ readChan ch
    liftIO reg
    return a

