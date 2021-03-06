{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
module LGtk.Effects where

import qualified Data.Time.Clock as Time
import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified System.Environment as Env
import System.IO.Error (catchIOError, isDoesNotExistError)

import LensRef.Context
import LensRef

--------------------------------------------------------------------------

liftEffectM = lift . lift

newtype Rt (m :: * -> *) a = Rt { runRt :: ReaderT (RefWriter (Rt m) () -> m (), SimpleRef m Time.UTCTime) m a }
    deriving (Monad, Applicative, Functor, MonadFix)

instance RefContext m => RefContext (Rt m) where
    type SimpleRef (Rt m) = SimpleRef m
    newSimpleRef = lift . newSimpleRef
    readSimpleRef = lift . readSimpleRef
    writeSimpleRef r = lift . writeSimpleRef r

instance MonadTrans Rt where
    lift = Rt . lift

type RefCreatorPost m = RefCreator (Rt m)

runRefCreatorPost
    :: (RefContext m, MonadBaseControl IO m, MonadFix m)
    => (m () -> m ())
    -> ((RefWriter (Rt m) () -> m ()) -> RefCreatorPost m a)
    -> m (a, m ())
runRefCreatorPost w f = mdo
    t <- liftIO_ Time.getCurrentTime
    r <- newSimpleRef t
    (a, runWr) <- flip runReaderT (runWr, r) . runRt $ runRefCreator $ \runWriter -> do
        let runWr = w . flip runReaderT (runWr, r) . runRt . runWriter
        a <- f runWr
        return (a, runWr)
    return $ (,) a $ do
        t <- liftIO_ Time.getCurrentTime
        writeSimpleRef r t

askPostpone = lift $ Rt $ asks fst


time     :: (RefContext m, MonadBaseControl IO m) => RefCreatorPost m Time.UTCTime
time = do
    r <- lift $ Rt $ asks snd
    liftEffectM $ readSimpleRef r

-- | The program's command line arguments (not including the program name).
getArgs     :: (RefContext m, MonadBaseControl IO m) => RefCreatorPost m [String]
getArgs     = liftIO' Env.getArgs

-- | The name of the program as it was invoked.
getProgName :: (RefContext m, MonadBaseControl IO m) => RefCreatorPost m String
getProgName = liftIO' Env.getProgName

-- | @(lookupEnv var)@ returns the value of the environment variable @var@.
lookupEnv   :: (RefContext m, MonadBaseControl IO m) => String -> RefCreatorPost m (Maybe String)
--    lookupEnv   = Env.lookupEnv -- does not work with Haskell Platform 2013.2.0.0
lookupEnv v = liftIO' $ catchIOError (fmap Just $ Env.getEnv v) $ \e ->
    if isDoesNotExistError e then pure Nothing else ioError e

{- |
@(asyncWrite t f a)@ has the effect of doing @(f a)@ after waiting @t@ milliseconds.

Note that @(asyncWrite 0 f a)@ acts immediately after the completion of the current computation,
so it is safe, because the effect of @(f a)@ is not interleaved with
the current computation.
Although @(asyncWrite 0)@ is safe, code using it has a bad small.
-}
asyncWrite :: (RefContext m, MonadBaseControl IO m) => Int -> RefWriter (Rt m) () -> RefCreatorPost m ()
asyncWrite t r = do
    (u, f) <- liftEffectM forkIOs'
    post <- askPostpone
    onRegionStatusChange $ lift . u
    liftEffectM $ f [ liftIO_ $ threadDelay t, post r ]

atTime :: (RefContext m, MonadBaseControl IO m) => Time.UTCTime -> RefWriter (Rt m) () -> RefCreatorPost m ()
atTime t m = do
    t' <- liftIO' Time.getCurrentTime
    asyncWrite (round $ 1000000 * Time.diffUTCTime t t') m

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
fileRef    :: (RefContext m, MonadBaseControl IO m) => FilePath -> RefCreatorPost m (Ref (Rt m) (Maybe String))
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

{- | Read a line from the standard input device.
@(getLine_ f)@ returns immediately. When the line @s@ is read,
@f s@ is called.
-}
getLine_   :: (RefContext m, MonadBaseControl IO m) => (String -> RefWriter (Rt m) ()) -> RefCreatorPost m ()
getLine_ w = do
    (u, f) <- liftEffectM forkIOs'
    post <- askPostpone
    onRegionStatusChange $ lift . u
    liftEffectM $ f [ liftIO_ getLine >>= post . w ]   -- TODO

-- | Write a string to the standard output device.
putStr_    :: (RefContext m, MonadBaseControl IO m) => String -> RefCreatorPost m ()
putStr_ s = liftIO' $ putStr s

-- | @putStrLn_@ === @putStr_ . (++ "\n")@
putStrLn_ :: (RefContext m, MonadBaseControl IO m) => String -> RefCreatorPost m ()
putStrLn_ = putStr_ . (++ "\n")


{-
getLine__ :: (String -> IO ()) -> IO (RegionStatusChange -> IO ())
getLine__ f = do
    _ <- forkIO $ forever $ getLine >>= f   -- todo
    pure $ const $ pure ()
-}
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
