{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.EffRef
    ( EffRef (..)
    , SafeIO (..)
    , EffIORef (..)
    , putStrLn_
    , forkIOs
    ) where

import Control.Concurrent
import Control.Exception (evaluate)
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

-- | Monad for dynamic actions
class ExtRef m => EffRef m where

    liftEffectM' :: Morph (EffectM m) m

    {- |
    Let @r@ be an effectless action (@ReadRef@ guarantees this).

    @(onChange init r fmm)@ has the following effect:

    Whenever the value of @r@ changes (with respect to the given equality),
    @fmm@ is called with the new value @a@.
    The value of the @(fmm a)@ action is memoized,
    but the memoized value is run again and again.

    The boolean parameter @init@ tells whether the action should
    be run in the beginning or not.

    For example, let @(k :: a -> m b)@ and @(h :: b -> m ())@,
    and suppose that @r@ will have values @a1@, @a2@, @a3@ = @a1@, @a4@ = @a2@.

    @onChange True r $ \\a -> k a >>= return . h@

    has the effect

    @k a1 >>= \\b1 -> h b1 >> k a2 >>= \\b2 -> h b2 >> h b1 >> h b2@

    and

    @onChange False r $ \\a -> k a >>= return . h@

    has the effect

    @k a2 >>= \\b2 -> h b2 >> k a1 >>= \\b1 -> h b1 >> h b2@
    -}
    onChange :: Eq a => Bool -> ReadRef m a -> (a -> m (m ())) -> m ()

    toReceive :: (a -> WriteRef m ()) -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m (Command -> EffectM m ())

    rEffect  :: (EffRef m, Eq a) => Bool -> ReadRef m a -> (a -> EffectM m ()) -> m ()

-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m, MonadRegister m, ExtRef (EffectM m), Ref m ~ Ref (EffectM m)) => EffRef (IdentityT m) where

    liftEffectM' = liftEffectM

    onChange init = toSend_ init . liftReadRef

    toReceive fm = toReceive_ (liftWriteRef . fm)

    rEffect init r f = onChange init r $ return . liftEffectM' . f

-- | Type class for IO actions.
class (EffRef m, SafeIO m, SafeIO (ReadRef m)) => EffIORef m where

    {- |
    @(asyncWrite t f a)@ has the effect of doing @(f a)@ after waiting @t@ milliseconds.

    Note that @(asyncWrite 0 f a)@ acts immediately after the completion of the current computation,
    so it is safe, because the effect of @(f a)@ is not interleaved with
    the current computation.
    Although @(asyncWrite 0)@ is safe, code using it has a bad small.
    -}
    asyncWrite :: Eq a => Int -> (a -> WriteRef m ()) -> a -> m ()

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

    -- | Write a string to the standard output device.
    putStr_    :: String -> m ()

    {- | Read a line from the standard input device.
    @(getLine_ f)@ returns immediately. When the line @s@ is read,
    @f s@ is called.
    -}
    getLine_   :: (String -> WriteRef m ()) -> m ()

    registerIO :: (a -> WriteRef m ()) -> ((a -> IO ()) -> IO (Command -> IO ())) -> m ()

    liftIO' :: IO a -> m a

-- | @putStrLn_@ === @putStr_ . (++ "\n")@
putStrLn_ :: EffIORef m => String -> m ()
putStrLn_ = putStr_ . (++ "\n")



-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m, MonadRegister m, ExtRef (EffectM m), Ref m ~ Ref (EffectM m), MonadIO' (EffectM m), SafeIO (ReadRef m), SafeIO m) => EffIORef (IdentityT m) where

    registerIO r fm = do
        _ <- toReceive r $ \x -> unliftIO $ \u -> liftM (fmap liftIO) $ liftIO $ fm $ u . x
        return ()

    liftIO' m = liftEffectM $ liftIO m

    asyncWrite t r a
        = registerIO r $ \re -> forkIOs [ threadDelay t, re a ]

    putStr_ = liftIO' . putStr

    getLine_ w = registerIO w $ \re -> do
        _ <- forkIO $ getLine >>= re
        return $ const $ return ()  -- TODO

    fileRef f = do
        ms <- liftIO' r
        ref <- newRef ms
        v <- liftIO' newEmptyMVar
        vman <- liftIO' newEmptyMVar
        cf <- liftIO' $ canonicalizePath f
        let
            cf' = decodeString cf
            g = (== cf')

            h = tryPutMVar v () >> return ()

            filt (Added x _) = g x
            filt (Modified x _) = g x
            filt (Removed x _) = g x

            act (Added _ _) = h
            act (Modified _ _) = h
            act (Removed _ _) = h

            startm = do
                man <- startManager
                watchDir man (directory cf') filt act
                putMVar vman $ stopManager man

        liftIO' startm
        registerIO (writeRef ref) $ \re -> forkForever $ takeMVar v >> r >>= re
        rEffect False (readRef ref) $ \x -> liftIO $ do
            join $ takeMVar vman
            _ <- tryTakeMVar v
            w x
            startm
        return ref
     where
        r = do
            b <- doesFileExist f
            if b then do
                xs <- readFile f
                _ <- evaluate (length xs)
                return (Just xs)
             else return Nothing

        w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)


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
        f _ Block = takeMVar x
        f _ Unblock = putMVar x ()

    liftM f $ forkIO $ g ios



