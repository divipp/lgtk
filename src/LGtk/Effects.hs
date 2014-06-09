{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module LGtk.Effects where

import Control.Applicative
import Control.Concurrent
--import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Fix
import Control.Monad.Operational
import Control.Monad.Trans.Control
--import System.Directory
--import qualified System.FilePath as F
--import System.FSNotify
--import Filesystem.Path hiding (FilePath)
--import Filesystem.Path.CurrentOS hiding (FilePath)
import qualified System.Environment as Env
import System.IO.Error (catchIOError, isDoesNotExistError)

import Data.LensRef.Class
import Data.LensRef.Common
import Data.LensRef.Default

--------------------------------------------------------------------------

-- | Type class for IO actions.
class MonadRefCreator m => EffIORef m where

    -- | The program's command line arguments (not including the program name). 
    getArgs     :: m [String]

    -- | The name of the program as it was invoked.
    getProgName :: m String

    -- | @(lookupEnv var)@ returns the value of the environment variable @var@.
    lookupEnv   :: String -> m (Maybe String)

    {- |
    @(asyncWrite t f a)@ has the effect of doing @(f a)@ after waiting @t@ milliseconds.

    Note that @(asyncWrite 0 f a)@ acts immediately after the completion of the current computation,
    so it is safe, because the effect of @(f a)@ is not interleaved with
    the current computation.
    Although @(asyncWrite 0)@ is safe, code using it has a bad small.
    -}
    asyncWrite :: Int -> RefWriter m () -> m ()

    {- |
    @(fileRef path)@ returns a reference which holds the actual contents
    of the file accessed by @path@.

    When the value of the reference changes, the file changes.
    When the file changes, the value of the reference changes.

    If the reference holds @Nothing@, the file does not exist.
    Note that you delete the file by putting @Nothing@ into the reference.    

    Implementation note: The references returned by @fileRef@ are not
    memoised so currently it is unsafe to call @fileRef@ on the same filepath more than once.
    This restriction will be lifted in the future.
    -}
    fileRef    :: FilePath -> m (Ref m (Maybe String))


    {- | Read a line from the standard input device.
    @(getLine_ f)@ returns immediately. When the line @s@ is read,
    @f s@ is called.
    -}
    getLine_   :: (String -> RefWriter m ()) -> m ()

    -- | Write a string to the standard output device.
    putStr_    :: EffIORef m => String -> m ()

-- | @putStrLn_@ === @putStr_ . (++ "\n")@
putStrLn_ :: EffIORef m => String -> m ()
putStrLn_ = putStr_ . (++ "\n")



data IOInstruction a where
    GetArgs :: IOInstruction [String]
    GetProgName :: IOInstruction String
    LookupEnv :: String -> IOInstruction (Maybe String)
    PutStr :: String -> IOInstruction ()
    GetLine :: (String -> SIO ()) -> IOInstruction Handle    
    AsyncWrite :: Int -> SIO () -> IOInstruction Handle
    FileRef :: FilePath -> (Maybe String -> SIO ()) -> IOInstruction (Handle, String -> SIO ())

type SIO = Program IOInstruction

type Handle = RegionStatusChange -> SIO ()

type RefCreatorPost m = ReaderT (RefWriter (RefCreator m) () -> m ()) (RefCreator m)

instance (MonadBaseControl IO m, NewRef m, n ~ RefWriter (RefCreator m))
    => EffIORef (ReaderT (n () -> m ()) (RefCreator m)) where

    getArgs     = liftIO' Env.getArgs

    getProgName = liftIO' Env.getProgName

--    lookupEnv   = Env.lookupEnv -- does not work with Haskell Platform 2013.2.0.0
    lookupEnv v = liftIO' $ catchIOError (fmap Just $ Env.getEnv v) $ \e ->
        if isDoesNotExistError e then pure Nothing else ioError e

    asyncWrite t r = do
        (u, f) <- liftEffectM forkIOs'
        post <- ask
        onRegionStatusChange u
        liftEffectM $ f [ liftIO_ $ threadDelay t, post r ]

    fileRef _ = newRef Nothing
{-
    fileRef f = do
        ms <- liftIO' r
        ref <- newRef ms
        v <- liftIO' newEmptyMVar
        vman <- liftIO' $ newMVar $ pure ()
        cf <- liftIO' $ canonicalizePath' f
        let
            cf' = decodeString cf
            g = (== cf')

            h = tryPutMVar v () >> pure ()

            filt (Added x _) = g x
            filt (Modified x _) = g x
            filt (Removed x _) = g x

            act (Added _ _) = h
            act (Modified _ _) = h
            act (Removed _ _) = h

            startm = do
                man <- startManager
                putMVar vman $ stopManager man
                watchDir man (directory cf') filt act

        (u, ff) <- liftEffectM  forkIOs'
        post <- ask
        onRegionStatusChange u
        liftEffectM $ ff $ repeat $ liftIO_ (takeMVar v >> r) >>= post . writeRef ref

        _ <- onChangeEq (readRef ref) $ \x -> liftIO' $ do
            join $ takeMVar vman
            _ <- tryTakeMVar v
            w x
            threadDelay 10000
            startm
        pure ref
     where
        r = do
            b <- doesFileExist f
            if b then do
                xs <- readFile f
                _ <- evaluate (length xs)
                pure (Just xs)
             else pure Nothing

        w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)
-}
    getLine_ w = do
        (u, f) <- liftEffectM forkIOs'
        post <- ask
        onRegionStatusChange u
        liftEffectM $ f [ liftIO_ getLine >>= post . w ]   -- TODO
    putStr_ s = liftIO' $ putStr s

getLine__ :: (String -> IO ()) -> IO (RegionStatusChange -> IO ())
getLine__ f = do
    _ <- forkIO $ forever $ getLine >>= f   -- todo
    pure $ const $ pure ()
{-
-- canonicalizePath may fail if the file does not exsist
canonicalizePath' p = fmap (F.</> f) $ canonicalizePath d 
  where (d,f) = F.splitFileName p
-}
liftIO' = liftEffectM . liftIO_

liftIO_ = liftBaseWith . const

forkIOs' :: MonadBaseControl IO m => m (RegionStatusChange -> m (), [m ()] -> m ())
forkIOs' = liftBaseWith $ \run -> do
    x <- newMVar ()
    s <- newEmptyMVar
    let g = do
            readMVar x
            is <- takeMVar s
            case is of
                [] -> pure ()
                (i:is) -> do
                    putMVar s is
                    _ <- run i
                    g
        f i Kill = killThread i
        f _ Block = takeMVar x
        f _ Unblock = putMVar x ()

    i <- forkIO g
    pure (liftIO_ . f i, liftIO_ . putMVar s)


