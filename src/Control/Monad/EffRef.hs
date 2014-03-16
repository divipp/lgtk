{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.EffRef where

import Control.Applicative
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import System.Directory
import System.FSNotify
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)
import Control.Monad.Operational

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef

-- | Monad for dynamic actions
class ExtRef m => EffRef m where

    type CallbackM m :: * -> *

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

--    toReceive :: Eq a => (a -> WriteRef m ()) -> ((a -> EffectM m ()) -> EffectM m (Command -> EffectM m ())) -> m (Command -> EffectM m ())
    toReceive :: Eq a => (a -> WriteRef m ()) -> (Command -> EffectM m ()) -> m (a -> CallbackM m ())

rEffect  :: (EffRef m, Eq a) => Bool -> ReadRef m a -> (a -> EffectM m ()) -> m ()
rEffect init r f = onChange init r $ return . liftEffectM' . f


type SyntEffRef m x = Program (EffRefI m x)
data EffRefI m x a where
    SyntLiftEffect :: m a -> EffRefI m x a
    SyntLiftExtRef :: SyntExtRef x a -> EffRefI m x a
    SyntOnChange :: Eq a => Bool -> SyntRefReader x a -> (a -> SyntEffRef m x (SyntEffRef m x ())) -> EffRefI m x ()
    SyntReceive  :: Eq a => (a -> SyntRefState x ()) -> (Command -> m ()) -> EffRefI m x (a -> CompEffRef m x ())

instance ExtRef (SyntEffRef m x) where
    type Ref (SyntEffRef m x) = SyntRef x
    extRef r l a = singleton $ SyntLiftExtRef $ extRef r l a
    newRef a = singleton $ SyntLiftExtRef $ newRef a

type CompEffRef m x = Program (CompEffRefI m x)
data CompEffRefI m x a where
    CompEffect :: m a -> CompEffRefI m x a
    CompLiftEffRef :: SyntEffRef m x a -> CompEffRefI m x a
    CompAtomically :: CompEffRef m x a -> CompEffRefI m x a

instance Monad m => MonadRegister (SyntEffRef m x) where
    type EffectM (SyntEffRef m x) = m

instance EffRef (SyntEffRef m x) where
    type CallbackM (SyntEffRef m x) = CompEffRef m x
    liftEffectM' = singleton . SyntLiftEffect
    onChange b r f = singleton $ SyntOnChange b r f
    toReceive f g = singleton $ SyntReceive f g

type SyntIORef x = Program (SyntIORefI x)
data SyntIORefI x a where
    SyntAsyncWrite :: Eq a => Int -> (a -> SyntRefState x ()) -> a -> SyntIORefI x ()
    SyntFileRef :: FilePath -> SyntIORefI x (SyntRef x (Maybe String))
    SyntGetLine :: (String -> SyntRefState x ()) -> SyntIORefI x ()
    SyntPutStr :: String -> SyntIORefI x ()

type SyntEffIORef x = SyntEffRef (SyntIORef x) x

instance SafeIO (SyntRefReader x) where
instance SafeIO (SyntEffIORef x) where

instance EffIORef (SyntEffIORef x) where
    asyncWrite_ d w a = singleton $ SyntLiftEffect $ singleton $ SyntAsyncWrite d w a
    fileRef f = singleton $ SyntLiftEffect $ singleton $ SyntFileRef f
    getLine_ r = singleton $ SyntLiftEffect $ singleton $ SyntGetLine r
    putStr_ s = singleton $ SyntLiftEffect $ singleton $ SyntPutStr s

runn :: SyntEffIORef x () -> CompEffRef (SyntIORef x) x ()
runn = undefined

{-
instance MonadRegister (SyntMonadRegister x) where
    type EffectM (SyntMonadRegister x) = SyntExtRef x
-}



-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m, MonadRegister m, ExtRef (EffectM m), Ref m ~ Ref (EffectM m)) => EffRef (IdentityT m) where

    type CallbackM (IdentityT m) = EffectM m

    liftEffectM' = liftEffectM

    onChange init = toSend_ init . liftReadRef

    toReceive fm = toReceive_ (liftWriteRef . fm)


-- | Type class for IO actions.
class (EffRef m, SafeIO m, SafeIO (ReadRef m)) => EffIORef m where


    {- |
    @(asyncWrite t f a)@ has the effect of doing @(f a)@ after waiting @t@ milliseconds.

    Note that @(asyncWrite 0 f a)@ acts immediately after the completion of the current computation,
    so it is safe, because the effect of @(f a)@ is not interleaved with
    the current computation.
    Although @(asyncWrite 0)@ is safe, code using it has a bad small.
    -}
    asyncWrite_ :: Eq a => Int -> (a -> WriteRef m ()) -> a -> m ()

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
    getLine_   :: (String -> WriteRef m ()) -> m ()

    -- | Write a string to the standard output device.
    putStr_    :: EffIORef m => String -> m ()

-- | @putStrLn_@ === @putStr_ . (++ "\n")@
putStrLn_ :: EffIORef m => String -> m ()
putStrLn_ = putStr_ . (++ "\n")

asyncWrite :: EffIORef m => Int -> (a -> WriteRef m ()) -> a -> m ()
asyncWrite t f a = asyncWrite' t $ f a

--registerIO :: Eq a => (a -> WriteRef m ()) -> (Command -> IO ()) -> m (a -> IO ())
registerIO r fm = do
        f <- toReceive r $ liftBase . fm -- $ \x -> unliftIO $ \u -> liftM (fmap liftBase) $ fm $ void . u . x
        liftEffectM' $ unliftIO' $ \u -> return $ u . f

asyncWrite' :: EffIORef m => Int -> WriteRef m () -> m ()
asyncWrite' t r = asyncWrite_ t (const r) ()


-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m, MonadRegister m, ExtRef (EffectM m), Ref m ~ Ref (EffectM m), MonadBaseControl IO (EffectM m), SafeIO (ReadRef m), SafeIO m) => EffIORef (IdentityT m) where

    putStr_ = liftIO' . putStr

    asyncWrite_ t r a = do
        (u, f) <- liftIO' forkIOs'
        x <- registerIO r u
        liftIO' $ f [ threadDelay t, x a ]

    getLine_ w = do
        (u, f) <- liftIO' forkIOs'
        x <- registerIO w u
        liftIO' $ f [ getLine >>= x ]   -- TODO

    fileRef f = do
        ms <- liftIO' r
        ref <- newRef ms
        v <- liftIO' newEmptyMVar
        vman <- liftIO' newEmptyMVar
        cf <- liftIO' $ canonicalizePath f   -- FIXME: canonicalizePath may fail if the file does not exsist
        let
            cf' = decodeString cf
            g = (== cf')

            h = tryPutMVar v () >> return ()

            filt (Added x _) = g x
            filt (Modified x _) = g x
            filt (Removed x _) = g x

            act (Added _ _) = putStrLn "added" >> h
            act (Modified _ _) = putStrLn "mod" >> h
            act (Removed _ _) = putStrLn "rem" >> h

            startm = do
                putStrLn " start" 
                man <- startManager
                putMVar vman $ putStrLn " stop" >> stopManager man
                watchDir man (directory cf') filt act

        liftIO' startm

        (u, ff) <- liftIO' forkIOs'
        re <- registerIO (writeRef ref) u
        liftIO' $ ff $ repeat $ takeMVar v >> r >>= re

        rEffect False (readRef ref) $ \x -> liftBase $ do
            join $ takeMVar vman
            _ <- tryTakeMVar v
            putStrLn "  write"
            w x
            threadDelay 10000
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


--liftIO' :: EffIORef m => IO a -> m a
liftIO' m = liftEffectM $ liftBase m

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

forkIOs' :: IO (Command -> IO (), [IO ()] -> IO ())
forkIOs' = do
    x <- newMVar ()
    s <- newEmptyMVar
    let g = do
            readMVar x
            is <- takeMVar s
            case is of
                [] -> return ()
                (i:is) -> do
                    putMVar s is
                    i
                    g
        f i Kill = killThread i
        f _ Block = takeMVar x
        f _ Unblock = putMVar x ()

    i <- forkIO g
    return (f i, putMVar s)

