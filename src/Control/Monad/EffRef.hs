{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.EffRef
    ( EffRef
    , EffIORef
    , fileRef
    , asyncWrite
    , register
    , onChange
    , rEffect
    , toSend, toReceive
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


type EffRef m = (ExtRef m, MonadRegister m, ExtRef (PureM m), Ref m ~ Ref (PureM m))

type EffIORef m = (EffRef m, EffectM m ~ IO, MonadIO m)

fileRef :: (EffIORef m) => FilePath -> m (Ref m (Maybe String))
fileRef f = do
    ms <- liftIO r
    ref <- newRef ms
    rEffect (readRef ref) $ w
    v <- liftIO $ do
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
    register ref $ \re -> forkForever $ takeMVar v >>= re
    return ref
 where
    r = do
        b <- doesFileExist f
        if b then do
            xs <- readFile f
            length xs `seq` return (Just xs)
         else return Nothing

    w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)

forkForever = forkIOs . repeat

forkIOs :: [IO ()] -> IO (Command -> IO ())
forkIOs ios = do
    x <- newMVar ()
    let g [] = return ()
        g (i:is) = do
            () <- takeMVar x
            putMVar x ()
            i
            g is
        f i Kill = killThread i
        f i Block = takeMVar x
        f i Unblock = putMVar x ()

    liftM f $ forkIO $ g ios

register :: (Eq a, EffRef m) => Ref m a -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m ()
register = toReceive . writeRef

asyncWrite :: (Eq a, EffIORef m) => Ref m a -> a -> Int -> m ()
asyncWrite r a t = register r $ \re -> forkIOs [ threadDelay t, re a ]

onChange :: (Eq a, EffRef m) => ReadRef m a -> (a -> m ()) -> m ()
onChange r f = toSend False r $ return . f

rEffect :: (EffRef m, Eq a) => ReadRef m a -> (a -> EffectM m ()) -> m ()
rEffect r f = onChange r $ liftEffectM . f

toSend :: (EffRef m, Eq b) => Bool -> ReadRef m b -> (b -> m (m ())) -> m ()
toSend b = toSend_ b . liftWriteRef . runR

toReceive :: (EffRef m, Eq a) => (a -> WriteRef m ()) -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m ()
toReceive fm = toReceive_ (liftWriteRef . fm)





