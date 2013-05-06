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
    , IC (..)
    , Receiver
    , mapReceiver
    , Sender
    , addCEffect
    , addFreeCEffect
    , addRefEffect
    , addPushEffect
    , constEffect
    , rEffect
    , fileRef

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
import Control.MLens.ExtRef --(IC (..))

type Receiver m a = (a -> Inn m ()) -> m ()

mapReceiver :: (a -> b) -> Receiver m a -> Receiver m b
mapReceiver f g h = g $ \a -> h $ f a

type Sender m a = ((a -> Inn m ()) -> Inn m ()) -> m ()

data IC m a = forall b . Eq b => IC (R (Inner' m) b) (b -> C m a)

class (Monad m, Monad (Inner' m), Monad (Inn m)) => MonadRegister m where

    type Inner' m :: * -> *
    type Inn m :: * -> *

    liftInn :: Morph (Inn m) m

    addICEffect :: Bool -> IC m a -> Receiver m a

    addWEffect :: Eq a => (a -> Inner' m ()) -> Sender m a

constEffect :: (MonadRegister m) => a -> Receiver m a 
constEffect a f = liftInn $ f a

addCEffect :: (MonadRegister m, Eq a) => R (Inner' m) a -> Receiver m a
addCEffect r = addICEffect False (IC r return)

rEffect :: (MonadRegister m, Eq a) => R (Inner' m) a -> Receiver m a
rEffect = addCEffect

addFreeCEffect :: (MonadRegister m, Functor (Inner' m), Eq a) => Free (R (Inner' m)) a -> Receiver m a
addFreeCEffect rb act = unFree (liftInn . act) (flip addCEffect act) rb

addRefEffect :: (MonadRegister m, Eq a, ExtRef m, Inner m ~ Inner' m) => IRef m a -> (a -> Inn m ()) -> ((a -> Inn m ()) -> Inn m ()) -> m ()
addRefEffect r act int = do
    addWEffect (writeRef r) int
    addCEffect (readRef r) act

addPushEffect :: MonadRegister m => Inner' m () -> (Inn m () -> Inn m ()) -> m ()
addPushEffect ma mb = addWEffect (const ma) $ \f -> mb $ f ()

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
    deriving (Functor, Monad)

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

evalEE :: (Monad n, MonadIO m) => Morph n IO -> Morph m IO -> EE n m a -> m a
evalEE morphN morph (EE m) = do
    vx <- liftIO $ newIORef $ return ()
    ch <- liftIO newChan
    (a, reg) <- runWriterT $ runReaderT m $ EEState (join $ readIORef vx) (writeChan ch) (MorphD morph) (MorphD morphN)
    liftIO $ writeIORef vx reg
    _ <- liftIO $ forkIO $ forever $ join $ readChan ch
    liftIO reg
    return a

instance (ExtRef m, n ~ Inner m) => ExtRef (EE n m) where

    type Ref (EE n m) = Ref m

    liftInner = EE . liftInner

    newRef = mapC EE . newRef

    extRef r k a = mapC EE $ extRef r k a

instance MonadIO m => MonadIO (EE n m) where

    liftIO m = EE $ liftIO m

unFree :: (Functor m, Monad m) => (a -> x) -> (m a -> x) -> Free m a -> x
unFree r m = evalFree r (m . join . fmap (induce id))

fileRef :: (ExtRef m, MonadRegister m, Inner m ~ Inner' m, MonadIO (Inn m)) => FilePath -> C m (IRef m (Maybe String))
fileRef f = unsafeC $ do
        ms <- liftInn $ liftIO r
        ref <- runC $ newRef ms
        addRefEffect ref (liftIO . w) $ \_cb -> return ()   -- TODO: use cb
        return ref
     where
        r = do
            b <- doesFileExist f
            if b then do
                xs <- readFile f
                length xs `seq` return (Just xs)
             else return Nothing

        w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)

