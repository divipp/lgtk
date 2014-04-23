{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.EffRef where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import System.Directory
import qualified System.FilePath as F
import System.FSNotify
import Filesystem.Path hiding (FilePath)
import Filesystem.Path.CurrentOS hiding (FilePath)
import Control.Monad.Operational

import Control.Monad.Restricted
import Control.Monad.ExtRef -- (ExtRef, extRef, newRef, Ref, WriteRef, ReadRef, liftWriteRef, liftReadRef, writeRef, readRef)

-- | Monad for dynamic actions
class (ExtRef m) => EffRef m where

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
    onChange_ :: Eq a => Bool -> WriteRef m a -> (a -> m (m ())) -> m ()

    toReceive_ :: (a -> WriteRef m ()) -> (Command -> EffectM m ()) -> m (a -> CallbackM m ())

onChange :: (EffRef m, Eq a) => Bool -> ReadRef m a -> (a -> m (m ())) -> m ()
onChange b r = onChange_ b (liftRefStateReader r)

toReceive :: EffRef m => (a -> WriteRef m ()) -> (Command -> EffectM m ()) -> m (a -> CallbackM m ())
toReceive = toReceive_

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)

rEffect  :: (EffRef m, Eq a) => Bool -> ReadRef m a -> (a -> EffectM m ()) -> m ()
rEffect init r f = onChange init r $ return . liftEffectM' . f


type SyntEffRef n m x = Program (EffRefI n m x)
data EffRefI n m x a where
    SyntLiftEffect :: m a -> EffRefI n m x a
    SyntLiftExtRef :: x a -> EffRefI n m x a
    SyntOnChange :: Eq a => Bool -> x a -> (a -> SyntEffRef n m x (SyntEffRef n m x ())) -> EffRefI n m x ()
    SyntReceive  :: (a -> x ()) -> (Command -> m ()) -> EffRefI n m x (a -> n ())

instance ExtRef x => ExtRef (SyntEffRef n m x) where
    type RefCore (SyntEffRef n m x) = RefCore x
    liftWriteRef w = singleton $ SyntLiftExtRef $ liftWriteRef w
    extRef r l a = singleton $ SyntLiftExtRef $ extRef r l a
    newRef a = singleton $ SyntLiftExtRef $ newRef a

--    type PureExt (SyntEffRef n m x) = PureExt x
    lazyExtRef r f = singleton $ SyntLiftExtRef $ lazyExtRef r $ liftM evalRegister' . evalRegister' . f

evalRegister'
    :: forall n m x a
    .  (Monad x)
    => SyntEffRef n m x a
    -> x a
evalRegister' = evalR
  where
    evalR :: SyntEffRef n m x b -> x b
    evalR = eval . view

    eval :: ProgramView (EffRefI n m x) b -> x b
    eval (Return x) = return x
    eval (SyntLiftExtRef m :>>= k) = m >>= evalR . k


liftEffectM = singleton . SyntLiftEffect

instance ExtRef x => EffRef (SyntEffRef n m x) where
    type EffectM (SyntEffRef n m x) = m
    type CallbackM (SyntEffRef n m x) = n
    liftEffectM' = singleton . SyntLiftEffect
    onChange_ b r f = singleton $ SyntOnChange b (liftWriteRef r) f
    toReceive_ f g = singleton $ SyntReceive (liftWriteRef . f) g


type Register m = WriterT (MonadMonoid m, Command -> MonadMonoid m) m

evalRegister
    :: forall n m x a
    .  (Monad n, NewRef m, Monad x)
    => MorphD x m
    -> (m () -> n ())
    -> SyntEffRef n m x a
    -> Register m a
evalRegister run ff = evalR
  where
    evalR :: SyntEffRef n m x b -> Register m b
    evalR = eval . view

    eval :: ProgramView (EffRefI n m x) b -> Register m b
    eval (Return x) = return x
    eval (SyntLiftEffect m :>>= k) = lift m >>= evalR . k
    eval (SyntLiftExtRef m :>>= k) = lift (runMorphD run m) >>= evalR . k
    eval (SyntReceive f g :>>= k) = tell w >> evalR (k $ ff . runMorphD run . f)
      where w = (mempty, MonadMonoid . g)
    eval (SyntOnChange b r f :>>= k)
        = toSend b (runMorphD run r) (liftM evalR . evalR . f) >>= evalR . k

toSend
    :: (Eq b, NewRef m)
    => Bool
    -> m b
    -> (b -> Register m (Register m ()))
    -> Register m ()
toSend init rb fb = do
        b <- lift rb
        v <- case init of
            False -> return $ Left b
            True -> lift $ do
                (c, (s1, ureg1)) <- runWriterT (fb b)
                (s2, ureg2) <- execWriterT c
                runMonadMonoid $ s1 `mappend` s2
                return $ Right [(b, (c, s1, s2, ureg1, ureg2))]
        memoref <- lift $ newRef' v
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

instance MonadIO m => SafeIO (SyntEffRef n m x) where
    getArgs = liftIO' getArgs
    getProgName = liftIO' getProgName
    lookupEnv = liftIO' . lookupEnv

instance (ExtRef x, MonadIO m) => EffIORef (SyntEffRef IO m x) where

    asyncWrite_ t r a = do
        (u, f) <- liftIO' forkIOs'
        x <- toReceive r $ liftIO . u
        liftIO' $ f [ threadDelay t, x a ]

    fileRef f = do
        ms <- liftIO' r
        ref <- newRef ms
        v <- liftIO' newEmptyMVar
        vman <- liftIO' newEmptyMVar
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

        liftIO' startm

        (u, ff) <- liftIO' forkIOs'
        re <- toReceive (writeRef ref) $ liftIO . u
        liftIO' $ ff $ repeat $ takeMVar v >> r >>= re

        rEffect False (readRef ref) $ \x -> liftIO $ do
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

--liftIO__ :: Monad m => m a -> SyntEffIORef m (State LSt) a
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
                    _ <- i
                    g
        f i Kill = killThread i
        f _ Block = takeMVar x
        f _ Unblock = putMVar x ()

    i <- forkIO g
    return (f i, putMVar s)

