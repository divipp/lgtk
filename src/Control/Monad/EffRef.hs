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
import Control.Monad.RWS
import Control.Monad.Writer

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import System.Directory
import System.FSNotify
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)
import Control.Monad.Operational

import Control.Monad.Restricted
import Control.Monad.ExtRef
import Control.Monad.ExtRef.Pure

-- | Monad for dynamic actions
class ExtRef m => EffRef m where

    type CallbackM m :: * -> *

    type EffectM m :: * -> *

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

    toReceive :: Eq a => (a -> WriteRef m ()) -> (Command -> EffectM m ()) -> m (a -> CallbackM m ())

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

rEffect  :: (EffRef m, Eq a) => Bool -> ReadRef m a -> (a -> EffectM m ()) -> m ()
rEffect init r f = onChange init r $ return . liftEffectM' . f


type SyntEffRef n m x = Program (EffRefI n m x)
data EffRefI n m x a where
    SyntLiftEffect :: m a -> EffRefI n m x a
    SyntLiftExtRef :: SyntExtRef x a -> EffRefI n m x a
    SyntOnChange :: Eq a => Bool -> SyntRefReader x a -> (a -> SyntEffRef n m x (SyntEffRef n m x ())) -> EffRefI n m x ()
    SyntReceive  :: Eq a => (a -> SyntRefState x ()) -> (Command -> m ()) -> EffRefI n m x (a -> n ())

instance ExtRef (SyntEffRef n m x) where
    type Ref (SyntEffRef n m x) = SyntRef x
    liftWriteRef w = singleton $ SyntLiftExtRef $ liftWriteRef w
    extRef r l a = singleton $ SyntLiftExtRef $ extRef r l a
    newRef a = singleton $ SyntLiftExtRef $ newRef a

liftEffectM = singleton . SyntLiftEffect

instance EffRef (SyntEffRef n m x) where
    type EffectM (SyntEffRef n m x) = m
    type CallbackM (SyntEffRef n m x) = n
    liftEffectM' = singleton . SyntLiftEffect
    onChange b r f = singleton $ SyntOnChange b r f
    toReceive f g = singleton $ SyntReceive f g


type CO m = WriterT (MonadMonoid m, Command -> MonadMonoid m) m

evalRegister' :: (NewRef m) => (StateT LSt m () -> m ()) -> SyntEffRef m (StateT LSt m) (Lens_ LSt) a -> CO (StateT LSt m) a
evalRegister' ff = eval . view
  where
    eval (Return x) = return x
    eval (SyntLiftEffect m :>>= k) = lift m >>= evalRegister' ff . k
    eval (SyntLiftExtRef m :>>= k) = lift (runExtRef m) >>= evalRegister' ff . k
    eval (SyntReceive f g :>>= k) = tell (t2 g) >> evalRegister' ff (k $ ff . runExtRef . liftWriteRef . f)
    eval (SyntOnChange b r f :>>= k) = toSend__ b (runExtRef $ liftReadRef r) (liftM (evalRegister' ff) . evalRegister' ff . f) >>= evalRegister' ff . k

newRef'' x = liftM (\r -> MorphD $ \m -> StateT $ \s -> runMorphD r $ mapStateT (\k -> runStateT k s >>= \((x, w), s) -> return ((x, s), w)) m) $ newRef' x

--toSend__ :: (Eq b, NewRef m) => Bool -> m b -> (b -> Register' m (Register' m ())) -> Register' m ()
toSend__ init rb fb = do
        b <- lift rb
        v <- case init of
            False -> return $ Left b
            True -> lift $ do
                (c, (s1, ureg1)) <- runWriterT (fb b)
                (s2, ureg2) <- execWriterT c
                runMonadMonoid $ s1 `mappend` s2
                return $ Right [(b, (c, s1, s2, ureg1, ureg2))]
        memoref <- lift $ lift $ newRef'' v
                            -- memo table, first item is the newest
        tell $ t1 $ do
            b <- rb
            join $ runMorphD memoref $ StateT $ \memo -> case memo of
                Left b' | b' == b -> return (return (), memo)
                Right ((b', (_, s1, s2, _, _)): _) | b' == b ->
                    return (runMonadMonoid $ s1 `mappend` s2, memo)
                _ -> do
                    case memo of
                        Right ((_, (_, _, _, ureg1, ureg2)): _) ->
                            runMonadMonoid $ ureg1 Block `mappend` ureg2 Kill
                        _ -> return ()
                    (c, (s1, ureg1)) <- case filter ((== b) . fst) $ either (const []) id memo of
                        ((_, (c, s1, _, ureg1, _)): _) -> do
                            runMonadMonoid $ ureg1 Unblock
                            return (c, (s1, ureg1))
                        _ -> runWriterT (fb b)
                    (s2, ureg2) <- execWriterT c
                    let memo' = Right $ (:) (b, (c, s1, s2, ureg1, ureg2)) $ filter ((/= b) . fst) $ either (const []) id memo
                    return (runMonadMonoid $ s1 `mappend` s2, memo')

t1 m = (MonadMonoid m, mempty)
t2 m = (mempty, MonadMonoid . m)


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

asyncWrite' :: EffIORef m => Int -> WriteRef m () -> m ()
asyncWrite' t r = asyncWrite_ t (const r) ()

type SyntEffIORef m x = SyntEffRef m (StateT LSt m) x

instance SafeIO (SyntRefReader x) where
instance SafeIO (SyntEffIORef m x) where

instance EffIORef (SyntEffIORef IO x) where

    asyncWrite_ t r a = do
        (u, f) <- liftIO' forkIOs'
        x <- toReceive r $ liftIO . u
        liftIO' $ f [ threadDelay t, x a ]

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
        re <- toReceive (writeRef ref) $ liftIO . u
        liftIO' $ ff $ repeat $ takeMVar v >> r >>= re

        rEffect False (readRef ref) $ \x -> liftIO $ do
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

    getLine_ w = do
        (u, f) <- liftIO' forkIOs'
        x <- toReceive w $ liftIO . u
        liftIO' $ f [ getLine >>= x ]   -- TODO
    putStr_ s = liftIO' $ putStr s

liftIO__ :: Monad m => m a -> SyntEffIORef m (Lens_ LSt) a
liftIO__ m = singleton $ SyntLiftEffect $ lift m

--liftIO' :: EffIORef m => IO a -> m a
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
                    i
                    g
        f i Kill = killThread i
        f _ Block = takeMVar x
        f _ Unblock = putMVar x ()

    i <- forkIO g
    return (f i, putMVar s)

