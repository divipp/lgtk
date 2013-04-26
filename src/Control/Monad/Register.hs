{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Register
    ( MonadRegister (..)
    , IC (..)
    , addFreeCEffect
    , addRefEffect
    , addPushEffect
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
import Data.MLens.Ref
import Control.MLens.ExtRef

data IC m a = forall b . Eq b => IC (R (Inner m) b) (b -> C m a)

class (NewRef m, Monad (Inn m)) => MonadRegister m where

    type Inn m :: * -> *

    liftInn :: Morph (Inn m) m

    update :: m ()

    addCEffect :: Eq a => C (Inner m) a -> (a -> Inn m ()) -> m ()

    addWEffect :: Eq a => (a -> Inner m ()) -> ((a -> Inn m ()) -> Inn m x) -> m x

    addICEffect :: IC m a -> Inn m (a -> Inn m ()) -> m ()

    addMemoICEffect :: IC m a -> Inn m (a -> Inn m ()) -> m ()


addFreeCEffect :: (MonadRegister m, Functor (Inner m), Eq a) => Free (C (Inner m)) a -> (a -> Inn m ()) -> m ()
addFreeCEffect rb act = unFree (liftInn . act) (flip addCEffect act) rb

addRefEffect :: (MonadRegister m, Eq a) => Ref (Inner m) a -> ((a -> Inn m ()) -> Inn m (a -> Inn m ())) -> m ()
addRefEffect r int = do
    act <- addWEffect (writeRef r) int
    addCEffect (rToC $ readRef r) act

addPushEffect :: MonadRegister m => Inner m () -> (Inn m () -> Inn m ()) -> m ()
addPushEffect ma mb = addWEffect (const ma) $ \f -> mb $ f ()


newtype EE m a = EE { unEE :: ReaderT (IRef m (m ()), IRef m (m ())) m a }
    deriving (Functor, Monad)

register m = EE $ do
    (_, r) <- ask
    lift $ liftInner $ modRef r (>> m)


instance (NewRef m) => MonadRegister (EE m) where

    type Inn (EE m) = m

    liftInn = EE . lift

    update = do
        (rr, _) <- EE ask
        acts <- liftInner $ runR $ readRef rr
        liftInn acts

    addWEffect r int = do
        (rr, _) <- EE ask
        liftInn $ int $ \a -> do
            liftInner $ r a
            join $ liftInner $ runR $ readRef rr

    addCEffect rb act = do
        lastB <- runC $ newRef Nothing
        register $ do
                b <- liftInner $ runC rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        liftInner $ writeRef lastB $ Just b
                        act b

    addICEffect (IC rb fb) int = do
        (rr, _) <- EE ask
        ir <- liftInn $ runC $ newRef $ return ()
        lastB <- liftInn $ runC $ newRef Nothing
        act <- liftInn int
        register $ do
                b <- liftInner $ runR rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        liftInner $ writeRef lastB $ Just b
                        liftInner $ writeRef ir $ return ()
                        c <- runReaderT (unEE $ runC $ fb b) (rr, ir)
                        act c
        register $ join $ liftInner $ runR $ readRef ir

    -- TODO: do not track events of inactive parts
    addMemoICEffect (IC rb fb) int = do
        (rr, _) <- EE ask
        ir <- liftInn $ runC $ newRef $ return ()
        lastB <- liftInn $ runC $ newRef Nothing
        prev <- liftInn $ runC $ newRef []
        act <- liftInn int
        register $ do
                b <- liftInner $ runR rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        liftInner $ writeRef lastB $ Just b
                        prevs <- liftInner $ runR $ readRef prev
                        c <- case [x | (b', x) <- prevs, b' == b] of
                            [(c, s)] -> do
                                liftInner $ writeRef ir s
                                return c
                            [] -> do
                                liftInner $ writeRef ir $ return ()
                                c <- runReaderT (unEE $ runC $ fb b) (rr, ir)
                                s <- liftInner $ runR $ readRef ir
                                liftInner $ modRef prev ((b, (c, s)) :)
                                return c
                        act c
        register $ join $ liftInner $ runR $ readRef ir

evalEE :: forall m a . NewRef m => (Morph (EE m) m -> EE m a) -> m a
evalEE f = do
    vx <- runC $ newRef $ return ()
    let unlift :: Morph (EE m) m
        unlift (EE m) = runReaderT m (vx, vx)
    unlift $ f unlift

instance NewRef m => NewRef (EE m) where

    type Inner (EE m) = Inner m

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

