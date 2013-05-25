{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( -- * Auxiliary definitions
      Morph
    , MorphD (..)
    , HasReadPart (..)
    , Ext (..), lift', runExt
    , MonadIO' (..)
    , NewRef (..)
    ) where

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

type Morph m n = forall a . m a -> n a

newtype MorphD m n = MorphD { runMorphD :: Morph m n }

class (Monad m, Monad (ReadPart m)) => HasReadPart m where

    type ReadPart m :: * -> *

    runR :: Morph (ReadPart m) m

instance Monad m => HasReadPart (StateT s m) where
    type ReadPart (StateT s m) = Reader s
    runR = gets . runReader


newtype Ext n m a = Ext { unExt :: ReaderT (MorphD n m) m a }
    deriving (Monad, MonadIO)

deriving instance MonadIO' (Ext n IO)

instance MonadTrans (Ext n) where
    lift = Ext . lift

lift' :: Monad m => n a -> Ext n m a
lift' m = Ext $ do
    r <- ask
    lift $ runMorphD r m

runExt :: MorphD n m -> Ext n m a -> m a
runExt v (Ext m) = runReaderT m v

class MonadIO m => MonadIO' m where
    unliftIO :: ((m a -> IO a) -> m b) -> m b

instance MonadIO' IO where
    unliftIO f = f id

instance MonadIO' (ReaderT r IO) where
    unliftIO f = do
        x <- ask
        f $ \m -> runReaderT m x


class Monad m => NewRef m where
    newRef' :: forall a . a -> m (MorphD (State a) m)

instance NewRef IO where
    newRef' x = do
        vx <- liftIO $ newMVar x
        return $ MorphD $ \m -> liftIO $ modifyMVar vx $ return . swap . runState m
      where
        swap (a, b) = (b, a)

instance NewRef m => NewRef (Ext n m) where
    newRef' a = liftM (\m -> MorphD $ lift . runMorphD m) $ lift $ newRef' a



