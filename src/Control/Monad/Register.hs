{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Free
import System.Directory

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

data EEState m = EEState
    { actions :: IRef m (m ())
    , registered :: IRef m (m ())
    }

newtype EE m a = EE { unEE :: ReaderT (EEState m) m a }
    deriving (Functor, Monad)

register :: (NewRef m) => m () -> EE m ()
register m = EE $ do
    r <- asks registered
    lift $ liftInner $ modRef r (>> m)

act :: (NewRef m) => EE m (m ())
act = EE $ do
    rr <- asks actions
    return $ join $ liftInner $ runR $ readRef rr

instance (NewRef m) => MonadRegister (EE m) where

    type Inn (EE m) = m

    liftInn = EE . lift

    update = act >>= liftInn

    addWEffect r int = do
        m <- act
        liftInn $ int $ \a -> do
            liftInner $ r a
            m

    -- TODO: do not track events of inactive parts
    addICEffect bb (IC rb fb) act = do
        rr <- EE ask
        ir <- liftInn $ runC $ newRef $ return ()
        lastB <- liftInn $ runC $ newRef Nothing
        prev <- liftInn $ runC $ newRef []
        register $ do
                b <- liftInner $ runR rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        liftInner $ writeRef lastB $ Just b
                        prevs <- liftInner $ runR $ readRef prev
                        act =<< case [x | (b', x) <- prevs, b' == b] of
                            [(c, s)] -> do
                                liftInner $ writeRef ir s
                                return c
                            [] -> do
                                liftInner $ writeRef ir $ return ()
                                c <- runReaderT (unEE $ runC $ fb b) $ rr { registered = ir }
                                s <- liftInner $ runR $ readRef ir
                                when bb $ liftInner $ modRef prev ((b, (c, s)) :)
                                return c
        register $ join $ liftInner $ runR $ readRef ir

evalEE :: forall m a . NewRef m => EE m a -> m a
evalEE (EE m) = do
    vx <- runC $ newRef $ return ()
    runReaderT m $ EEState vx vx

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

