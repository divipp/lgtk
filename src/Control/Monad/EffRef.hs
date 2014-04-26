{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.EffRef where

import Data.Monoid
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import System.Directory
import qualified System.FilePath as F
import System.FSNotify
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)
import qualified System.Environment as Env
import System.IO.Error (catchIOError, isDoesNotExistError)

import Control.Monad.ExtRef

writeRef' :: (EffRef m, Reference r, RefReader r ~ RefReader (RefCore m)) => MRef r a -> a -> Modifier m ()
writeRef' r a = liftModifier $ liftWriteRef $ writeRef r a

-- | @modRef r f@ === @liftRefStateReader (readRef r) >>= writeRef r . f@
--modRef :: Reference r => MRef r a -> (a -> a) -> RefStateReader (RefReader r) ()
r `modRef'` f = liftRefStateReader' (readRef r) >>= writeRef' r . f


liftRefStateReader' :: EffRef m => ReadRef m a -> Modifier m a
liftRefStateReader' r = liftModifier $ liftWriteRef $ liftRefStateReader r

action' m = liftModifier $ liftEffectM m

-- | Monad for dynamic actions
class (ExtRef m, ExtRef (Modifier m), RefCore (Modifier m) ~ RefCore m) => EffRef m where

    type CallbackM m :: * -> *

    type EffectM m :: * -> *

    data Modifier m a :: *

    liftEffectM :: EffectM m a -> m a

    liftModifier :: m a -> Modifier m a

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
    onChange_
        :: Eq b
        => ReadRef m b
        -> b -> (b -> c)
        -> (b -> b -> c -> m (c -> m c))
        -> m (ReadRef m c)

    toReceive :: Functor f => f (Modifier m ()) -> (Command -> EffectM m ()) -> m (f (CallbackM m ()))

onChange :: (EffRef m, Eq a) => ReadRef m a -> (a -> m (m b)) -> m (ReadRef m b)
onChange r f = onChange_ r undefined undefined $ \b _ _ -> liftM (\x _ -> x) $ f b

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

rEffect  :: (EffRef m, Eq a) => ReadRef m a -> (a -> EffectM m b) -> m (ReadRef m b)
rEffect r f = onChange r $ return . liftEffectM . f

type Register m = ReaderT (Ref m (MonadMonoid m, Command -> MonadMonoid m)) m

newtype Reg n m a = Reg (ReaderT (m () -> n ()) (Register m) a) deriving (Monad, Applicative, Functor)


instance ExtRef m => ExtRef (Modifier (Reg n m)) where

    type RefCore (Modifier (Reg n m)) = RefCore m

    liftWriteRef w = RegW $ liftWriteRef w
    extRef rr l a = RegW $ extRef rr l a
    newRef a = RegW $ newRef a

instance ExtRef m => ExtRef (Reg n m) where

    type RefCore (Reg n m) = RefCore m

    liftWriteRef w = Reg $ lift $ lift $ liftWriteRef w
    extRef rr l a = Reg $ lift $ lift $ extRef rr l a
    newRef a = Reg $ lift $ lift $ newRef a

instance (ExtRef m, Monad n) => EffRef (Reg n m) where

    type EffectM (Reg n m) = m

    type CallbackM (Reg n m) = n

    newtype Modifier (Reg n m) a = RegW {unRegW :: Reg n m a} deriving (Monad, Applicative, Functor)

    liftEffectM m = Reg $ lift $ lift $ m

    liftModifier m = RegW m

    onChange_ r b0 c0 f = Reg $ ReaderT $ \ff ->
        toSend r b0 c0 $ \b b' c' -> liftM (\x -> evalRegister ff . x) $ evalRegister ff $ f b b' c'

    toReceive f g = Reg $ ReaderT $ \ff -> do
        tell (mempty, MonadMonoid . g)
        writerstate <- ask
        return $ fmap (ff . flip runReaderT writerstate . evalRegister ff . unRegW) f

evalRegister
    :: (Monad n, ExtRef m)
    => (m () -> n ())
    -> Reg n m a
    -> Register m a
evalRegister ff (Reg m) = runReaderT m ff

toSend
    :: (Eq b, ExtRef m)
    => ReadRef m b
    -> b -> (b -> c)
    -> (b -> b -> c -> {-Either (Register m c)-} (Register m (c -> Register m c)))
    -> Register m (ReadRef m c)
toSend rb b0 c0 fb = do
    let doit st = readRef' st >>= runMonadMonoid . fst
        reg st msg = readRef' st >>= runMonadMonoid . ($ msg) . snd

    memoref <- lift $ do
        b <- liftReadRef rb
        (c, st1) <- runWriterT' $ fb b b0 $ c0 b0
        (val, st2) <- runWriterT' $ c $ c0 b0
        doit st1
        doit st2
        newRef ((b, (c, val, st1, st2)), [])      -- memo table

    let act = MonadMonoid $ do
            b <- liftReadRef rb
            (last@(b', cc@(_, oldval, st1, st2)), memo) <- readRef' memoref
            (_, _, st1, st2) <- if b' == b
              then
                return cc
              else do
                reg st1 Block
                reg st2 Kill
                (c, oldval', st1, _) <- case lookup b memo of
                  Nothing -> do
                    (c, st1) <- runWriterT' $ fb b b' oldval
                    return (c, c0 b, st1, undefined)
                  Just cc'@(_, _, st1, _) -> do
                    reg st1 Unblock
                    return cc'
                (val, st2) <- runWriterT' $ c oldval'
                let cc = (c, val, st1, st2)
                liftWriteRef $ writeRef memoref ((b, cc), filter ((/= b) . fst) (last:memo))
                return cc
            doit st1
            doit st2

    tell (act, mempty)
    return $ readRef $ (_1 . _2 . _2) `lensMap` memoref

----------------

-- Ref-based WriterT
type WriterT w m = ReaderT (Ref m w) m

runWriterT' :: (ExtRef m, Monoid w) => WriterT w m a -> m (a, Ref m w)
runWriterT' m = do
    r <- newRef mempty
    a <- runReaderT m r
    return (a, r)

runWriterT :: (ExtRef m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = do
    (a, r) <- runWriterT' m
    w <- readRef' r
    return (a, w)

tell :: (Monoid w, ExtRef m) => w -> WriterT w m ()
tell w = ReaderT $ \m -> readRef' m >>= liftWriteRef . writeRef m . (`mappend` w)




--------------------------------------------------------------------------

-- | Type class for IO actions.
class EffRef m => EffIORef m where

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
    asyncWrite :: Int -> Modifier m () -> m ()

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
    getLine_   :: (String -> Modifier m ()) -> m ()

    -- | Write a string to the standard output device.
    putStr_    :: EffIORef m => String -> m ()

-- | @putStrLn_@ === @putStr_ . (++ "\n")@
putStrLn_ :: EffIORef m => String -> m ()
putStrLn_ = putStr_ . (++ "\n")


instance (ExtRef m, MonadIO m) => EffIORef (Reg IO m) where


    getArgs     = liftIO' Env.getArgs

    getProgName = liftIO' Env.getProgName

--    lookupEnv   = Env.lookupEnv -- does not work with Haskell Platform 2013.2.0.0
    lookupEnv v = liftIO' $ catchIOError (liftM Just $ Env.getEnv v) $ \e ->
        if isDoesNotExistError e then return Nothing else ioError e

    asyncWrite t r = do
        (u, f) <- liftIO' forkIOs'
        x <- toReceive (const r) $ liftIO . u
        liftIO' $ f [ threadDelay t, x () ]

    fileRef f = do
        ms <- liftIO' r
        ref <- newRef ms
        v <- liftIO' newEmptyMVar
        vman <- liftIO' $ newMVar $ return ()
        cf <- liftIO' $ canonicalizePath' f
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
                putMVar vman $ stopManager man
                watchDir man (directory cf') filt act

        (u, ff) <- liftIO' forkIOs'
        re <- toReceive (writeRef' ref) $ liftIO . u
        liftIO' $ ff $ repeat $ takeMVar v >> r >>= re

        _ <- rEffect (readRef ref) $ \x -> liftIO $ do
            join $ takeMVar vman
            _ <- tryTakeMVar v
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

    getLine_ w = do
        (u, f) <- liftIO' forkIOs'
        x <- toReceive w $ liftIO . u
        liftIO' $ f [ getLine >>= x ]   -- TODO
    putStr_ s = liftIO' $ putStr s

-- canonicalizePath may fail if the file does not exsist
canonicalizePath' p = liftM (F.</> f) $ canonicalizePath d 
  where (d,f) = F.splitFileName p

liftIO__ m = liftEffectM $ lift m

liftIO' m = liftEffectM $ liftIO m


--forkIOs' :: IO (Command -> IO (), [IO ()] -> IO ())
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
                    _ <- i
                    g
        f i Kill = killThread i
        f _ Block = takeMVar x
        f _ Unblock = putMVar x ()

    i <- forkIO g
    return (f i, putMVar s)

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


