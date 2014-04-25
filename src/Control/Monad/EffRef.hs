{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.EffRef where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import System.Directory
import qualified System.FilePath as F
import System.FSNotify
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)

import Control.Monad.Restricted
import Control.Monad.ExtRef -- (ExtRef, extRef, newRef, Ref, WriteRef, ReadRef, liftWriteRef, liftReadRef, writeRef, readRef)

-- | Monad for dynamic actions
class (ExtRef m) => EffRef m where

    type CallbackM m :: * -> *

    type EffectM m :: * -> *

    liftEffectM :: Morph (EffectM m) m

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
    onChange :: Eq a => ReadRef m a -> (a -> m (m b)) -> m (ReadRef m b)

    toReceive :: Functor f => f (WriteRef m ()) -> (Command -> EffectM m ()) -> m (f (CallbackM m ()))

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

rEffect  :: (EffRef m, Eq a) => ReadRef m a -> (a -> EffectM m b) -> m (ReadRef m b)
rEffect r f = onChange r $ return . liftEffectM . f

type Register m = WriterT (MonadMonoid m, Command -> MonadMonoid m) m

newtype Reg n m a = Reg (ReaderT (m () -> n ()) (Register m) a) deriving (Monad, Applicative, Functor)

instance (ExtRef m) => ExtRef (Reg n m) where
    type RefCore (Reg n m) = RefCore m
    liftWriteRef w = Reg $ lift $ lift $ liftWriteRef w
    extRef rr l a = Reg $ lift $ lift $ extRef rr l a
    newRef a = Reg $ lift $ lift $ newRef a

instance (ExtRef m, Monad n, NewRef m) => EffRef (Reg n m) where
    type EffectM (Reg n m) = m
    type CallbackM (Reg n m) = n
    liftEffectM m = Reg $ lift $ lift $ m
    onChange r f = Reg $ ReaderT $ \ff ->
      toSend (liftReadRef r) (evalRegister ff . liftM (evalRegister ff) . f)
    toReceive f g = Reg $ ReaderT $ \ff -> tell w >> return (fmap (ff . liftWriteRef) f)
      where w = (mempty, MonadMonoid . g)

evalRegister
    :: (Monad n, NewRef m, ExtRef m)
    => (m () -> n ())
    -> Reg n m a
    -> Register m a
evalRegister ff (Reg m) = runReaderT m ff

toSend
    :: (Eq b, NewRef m, ExtRef m)
    => m b
    -> (b -> Register m (Register m c))
    -> Register m (ReadRef m c)
toSend rb fb = do
        b <- lift rb
        (c, (s1, ureg1)) <- lift $ runWriterT (fb b)
        (val, (s2, ureg2)) <- lift $ runWriterT c
        lift $ runMonadMonoid $ s1 `mappend` s2
        let v = [(b, (c, s1, s2, ureg1, ureg2))]
        memoref <- lift $ newRef' v
                            -- memo table, first item is the newest

        r <- lift $ newRef val
        tell $ t1 $ do
            b <- rb
            join $ runMorphD memoref $ StateT $ \memo -> case memo of
                ((b', (_, s1, s2, _, _)): _) | b' == b ->
                    return (runMonadMonoid $ s1 `mappend` s2, memo)
                _ -> do
                    case memo of
                        ((_, (_, _, _, ureg1, ureg2)): _) ->
                            runMonadMonoid $ ureg1 Block `mappend` ureg2 Kill
                        _ -> return ()
                    (c, (s1, ureg1)) <- case filter ((== b) . fst) memo of
                        ((_, (c, s1, _, ureg1, _)): _) -> do
                            runMonadMonoid $ ureg1 Unblock
                            return (c, (s1, ureg1))
                        _ -> runWriterT (fb b)
                    (val, (s2, ureg2)) <- runWriterT c
                    liftWriteRef $ writeRef r val
                    let memo' = (:) (b, (c, s1, s2, ureg1, ureg2)) $ filter ((/= b) . fst) memo
                    return (runMonadMonoid $ s1 `mappend` s2, memo')
        return $ readRef r
  where
    t1 m = (MonadMonoid m, mempty)


-- | Type class for IO actions.
class (EffRef m, SafeIO m) => EffIORef m where


    {- |
    @(asyncWrite t f a)@ has the effect of doing @(f a)@ after waiting @t@ milliseconds.

    Note that @(asyncWrite 0 f a)@ acts immediately after the completion of the current computation,
    so it is safe, because the effect of @(f a)@ is not interleaved with
    the current computation.
    Although @(asyncWrite 0)@ is safe, code using it has a bad small.
    -}
    asyncWrite :: Int -> WriteRef m () -> m ()

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

instance (MonadIO m, NewRef m, ExtRef m, Monad n) => SafeIO (Reg n m) where
    getArgs = liftIO' getArgs
    getProgName = liftIO' getProgName
    lookupEnv = liftIO' . lookupEnv

instance (ExtRef m, MonadIO m, NewRef m) => EffIORef (Reg IO m) where

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
        re <- toReceive (writeRef ref) $ liftIO . u
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

