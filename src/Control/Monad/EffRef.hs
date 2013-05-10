{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.EffRef
    ( EffRef
    , EffIORef
    , fileRef
    , async
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.FSNotify
--import System.FilePath
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)
import Prelude hiding ((.), id)

--import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef


type EffRef m = (ExtRef m, MonadRegister m, WriteRef m ~ PureM m)

type EffIORef m = (EffRef m, EffectM m ~ IO)

fileRef :: (EffIORef m) => FilePath -> m (Ref m (Maybe String))
fileRef f = do
    ms <- liftEffectM $ liftIO r
    ref <- newRef ms
    rEffect (readRef ref) $ liftIO . w
    v <- liftEffectM $ liftIO $ do
        v <- newEmptyMVar
        cf <- canonicalizePath f
        let
            cf' = decodeString cf
            g = (== cf')

            h = r >>= putMVar v

            filt (Added x _) = g x
            filt (Modified x _) = g x
            filt (Removed x _) = g x

            act (Added _ _) = h
            act (Modified _ _) = h
            act (Removed _ _) = h
        man <- startManager
        watchDir man (directory cf') filt act
        return v
    toReceive (writeRef ref) $ \re -> void $ forkIO $ forever $ takeMVar v >>= re
    return ref
 where
    r = do
        b <- doesFileExist f
        if b then do
            xs <- readFile f
            length xs `seq` return (Just xs)
         else return Nothing

    w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)

async
    :: (Eq a, EffIORef m)
    => Receive m a
    -> Send m a
    -> m ()
async r w = do
    v <- liftEffectM $ liftIO newEmptyMVar
    r $ \re -> void $ forkIO $ forever $ takeMVar v >>= re
    w $ putMVar v


