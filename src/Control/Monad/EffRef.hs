{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.EffRef
    ( EffRef (..)
    , SafeIO (..)
    , EffIORef (..)
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import System.Directory
import System.FSNotify
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)
import Prelude hiding ((.), id)

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef

class ExtRef m => EffRef m where

    onChange :: Eq a => ReadRef m a -> (a -> m (m ())) -> m ()

    register :: Eq a => Ref m a -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m ()
    register = toReceive . writeRef

    rEffect  :: Eq a => ReadRef m a -> (a -> EffectM m ()) -> m ()

    toReceive :: Eq a => (a -> WriteRef m ()) -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m ()

    constSend :: a -> (a -> EffectM m ()) -> m ()


instance (ExtRef m, MonadRegister m, ExtRef (EffectM m), Ref m ~ Ref (EffectM m)) => EffRef (IdentityT m) where

    rEffect r f = onChange r $ return . liftEffectM . f

    onChange = toSend_ . liftWriteRef . liftReadPart

    toReceive fm = toReceive_ (liftWriteRef . fm)

    constSend a f = liftEffectM $ f a


class (EffRef m, SafeIO m, SafeIO (ReadRef m)) => EffIORef m where

    asyncWrite :: Eq a => Ref m a -> a -> Int -> m ()
    fileRef    :: FilePath -> m (Ref m (Maybe String))
    putStr_    :: String -> m ()
    getLine_   :: (String -> WriteRef m ()) -> m ()

    registerIO :: Eq a => (a -> WriteRef m ()) -> ((a -> IO ()) -> IO (Command -> IO ())) -> m ()

instance (ExtRef m, MonadRegister m, ExtRef (EffectM m), Ref m ~ Ref (EffectM m), MonadIO' (EffectM m), SafeIO (ReadRef m), SafeIO m) => EffIORef (IdentityT m) where

    registerIO r fm
        = toReceive r $ \x -> unliftIO $ \u -> liftM (fmap liftIO) $ liftIO $ fm $ u . x

    asyncWrite r a t = registerIO (writeRef r) $ \re -> forkIOs [ threadDelay t, re a ]

    putStr_ = liftIO' . putStr

    getLine_ w = registerIO w $ \re -> do
        forkIO $ getLine >>= re
        return $ const $ return ()  -- TODO

    fileRef f = do
        ms <- liftIO' r
        ref <- newRef ms
        rEffect (readRef ref) $ liftIO . w
        v <- liftIO' $ do
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
        registerIO (writeRef ref) $ \re -> forkForever $ liftIO $ takeMVar v >>= re
        return ref
     where
        r = do
            b <- doesFileExist f
            if b then do
                xs <- readFile f
                length xs `seq` return (Just xs)
             else return Nothing

        w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)


--liftIO' :: EffIORef_ m => IO a -> m a
liftIO' m = liftEffectM $ liftIO m

forkForever :: IO () -> IO (Command -> IO ())
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



