{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Register
    ( MonadRegister (..)
    , Receiver
    , mapReceiver
    , Sender
    , addCEffect
    , addFreeCEffect
    , addRefEffect
    , addPushEffect
    , constEffect
    , rEffect
    , FileSystem (..)

    , EE
    , evalEE

    -- * Auxiliary definitions
    , unFree
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Free
import Control.Concurrent
import System.Directory
import Data.IORef
import Data.List

import Control.Monad.Restricted
import Control.MLens.ExtRef

type Receiver m a = (a -> Inn m ()) -> m ()

mapReceiver :: (a -> b) -> Receiver m a -> Receiver m b
mapReceiver f g h = g $ \a -> h $ f a

type Sender m a = ((a -> Inn m ()) -> Inn m ()) -> m ()

class (NewRef m, Monad (Inn m)) => MonadRegister m where

    type Inn m :: * -> *

    liftInn :: Morph (Inn m) m

    update :: m ()

    addICEffect :: Bool -> IC m a -> Receiver m a

    addWEffect :: Eq a => (a -> Inner m ()) -> ((a -> Inn m ()) -> Inn m x) -> m x

constEffect :: (MonadRegister m) => a -> Receiver m a 
constEffect a f = liftInn $ f a

addCEffect :: (MonadRegister m, Eq a) => R (Inner m) a -> Receiver m a
addCEffect r = addICEffect False (IC r return)

rEffect :: (MonadRegister m, Eq a) => R (Inner m) a -> Receiver m a
rEffect = addCEffect

addFreeCEffect :: (MonadRegister m, Functor (Inner m), Eq a) => Free (R (Inner m)) a -> Receiver m a
addFreeCEffect rb act = unFree (liftInn . act) (flip addCEffect act) rb

addRefEffect :: (MonadRegister m, Eq a) => IRef m a -> ((a -> Inn m ()) -> Inn m (a -> Inn m ())) -> m ()
addRefEffect r int = do
    act <- addWEffect (writeRef r) int
    addCEffect (readRef r) act

addPushEffect :: MonadRegister m => Inner m () -> (Inn m () -> Inn m ()) -> m ()
addPushEffect ma mb = addWEffect (const ma) $ \f -> mb $ f ()

data MorphD m n = MorphD (Morph m n)

data EEState m = EEState
    { actions :: IO ()
    , sendEvent :: IO () -> IO ()
    , morph :: MorphD m IO
    }

instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)

newtype EE m a = EE { unEE :: ReaderT (EEState m) (WriterT (IO ()) m) a }
    deriving (Functor, Monad)

act :: (MonadIO m) => EE m (IO ())
act = EE $ asks actions

morphD :: Monad m => EE m (MorphD m IO)
morphD = EE $ asks morph

unlift :: MorphD m n -> forall a . m a -> n a
unlift (MorphD f) m = f m

instance (NewRef m, MonadIO m) => MonadRegister (EE m) where

    type Inn (EE m) = IO

    liftInn = EE . liftIO

    update = act >>= liftInn

    addWEffect r int = do
        m <- act
        md <- morphD
        send <- EE $ asks sendEvent
        liftInn $ int $ \a -> send $ do
            unlift md $ liftInner $ r a
            m

    addICEffect bb (IC rb fb) act = do
        rr <- EE ask
        memoref <- liftIO $ newIORef []  -- memo table, first item is the newest
        md <- morphD
        EE $ tell $ do
            b <- unlift md $ liftInner $ runR rb
            join $ atomicModifyIORef' memoref $ \memo -> case memo of
                ((b', (_, s)): _) | b' == b -> (memo, s)
                _ -> case partition ((== b) . fst) memo of
                    (x@(_, (c, s)): _, rem) -> (x: rem, forkIO (act c) >> s)
                    _ -> (,) memo $ do
                        (c, s) <- unlift md $ runWriterT $ runReaderT (unEE $ runC $ fb b) rr
                        when bb $ atomicModifyIORef' memoref $ \memo -> ((b, (c, s)) : filter ((/= b) . fst) memo, ())
                        forkIO (act c) >> s

evalEE :: forall m a . (NewRef m, MonadIO m) => Morph m IO -> EE m a -> m a
evalEE morph (EE m) = do
    vx <- liftIO $ newIORef $ return ()
    ch <- liftIO newChan
    _ <- liftIO $ forkIO $ forever $ join $ readChan ch
    (a, reg) <- runWriterT $ runReaderT m $ EEState (join $ readIORef vx) (writeChan ch) $ MorphD morph
    liftIO $ writeIORef vx reg
    return a

instance NewRef m => NewRef (EE m) where

    type Ref (EE m) = Ref m

    liftInner = EE . liftInner

    newRef = mapC EE . newRef

instance ExtRef m => ExtRef (EE m) where

    extRef r k a = mapC EE $ extRef r k a

instance MonadIO m => MonadIO (EE m) where

    liftIO m = EE $ liftIO m

unFree :: (Functor m, Monad m) => (a -> x) -> (m a -> x) -> Free m a -> x
unFree r m = evalFree r (m . join . fmap (induce id))

class NewRef m => FileSystem m where
    -- | Note that if you write @Nothing@, the file is deleted.
    fileRef :: FilePath -> C m (IRef m (Maybe String))

instance (MonadIO m, NewRef m) => FileSystem (EE m) where

    fileRef f = unsafeC $ do
        ms <- liftInn $ liftIO r
        ref <- runC $ newRef ms
        addRefEffect ref $ \_cb -> return $ liftIO . w   -- TODO: use cb
        return ref
     where
        r = do
            b <- doesFileExist f
            if b then do
                xs <- readFile f
                length xs `seq` return (Just xs)
             else return Nothing

        w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)

