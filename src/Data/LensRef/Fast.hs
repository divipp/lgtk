{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
Fast reference implementation for the @ExtRef@ interface.

TODO
- elim mem leak: regitered events don't allow to release unused refs
- optimiziation: do not remember values
- optimiziation: equality check
- generalize it to be a monad transformer
-}
module Data.LensRef.Fast
    ( Pure
    , runPure
    ) where

import Data.Monoid
--import qualified Data.Map
import Control.Concurrent
import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Data.LensRef

----------------------

newtype instance RefWriterOf IO a
    = RefWriterOfIO { runRefWriterOfIO :: IO a }
        deriving (Monad, Applicative, Functor)

----------------------

data Lens_ a = Lens_ 
    { readPart :: IO a
    , writePart :: a -> IO ()
    , register :: IO () -> IO ()
    }

joinLens m = Lens_
    { readPart = m >>= readPart
    , writePart = \a -> m >>= \r -> writePart r a
    , register = \e -> m >>= \r -> register r e
    }

instance Reference Lens_ where
    type RefReader Lens_ = IO

    readRef_ = readPart . joinLens
    writeRef_ m = RefWriterOfIO . writePart (joinLens m)
    lensMap l m = do
        Lens_ r w t <- m
        return $ Lens_
            { readPart = r >>= \a -> return $ a ^. l
            , writePart = \b -> r >>= \a -> w $ set l b a
            , register = t
            }
    unitRef = return $ Lens_
                { readPart = return ()
                , writePart = const $ return ()
                , register = \_ -> return ()
                }

instance {- Monad m => -} MonadRefReader IO where

    type RefCore IO = Lens_

    liftReadRef = id

instance ExtRef IO where

    extRef r r2 a0 = do
        Lens_ rb wb tb <- r
        b0 <- rb
        va <- newMVar $ set r2 b0 a0
        reg <- newMVar $ return ()
        status <- newMVar True -- True: normal; False:
        tb $ do
            s <- readMVar status
            when s $ do
                b <- rb
                modifyMVar va $ \a -> return (set r2 b a, ())
                join $ readMVar reg
        return $ do
            return $ Lens_
                { readPart = readMVar va
                , writePart = \a -> do
                    _ <- swapMVar va a
                    _ <- swapMVar status False
                    wb $ a ^. r2
                    _ <- swapMVar status True
                    join $ readMVar reg
                , register = \m -> modifyMVar reg $ \x -> return (x >> m, ())
                }

    newRef a0 = do
        va <- newMVar a0
        reg <- newMVar $ return ()
        return $ return $ Lens_
                { readPart = readMVar va
                , writePart = \a -> do
                    _ <- swapMVar va a
                    join $ readMVar reg
                , register = \m -> modifyMVar reg $ \x -> return (x >> m, ())
                }

    memoRead = memoRead_

    memoWrite = memoWrite_

    future = future_


future_ :: (ExtRef m, MonadRefWriter m) => (ReadRef m a -> m a) -> m a
future_ f = do
    s <- newRef $ error "can't see the future"
    a <- f $ readRef s
    writeRef s a
    return a

memoRead_ g = do
    s <- newRef Nothing
    return $ readRef s >>= \x -> case x of
        Just a -> return a
        _ -> g >>= \a -> do
            writeRef s $ Just a
            return a

memoWrite_ g = do
    s <- newRef Nothing
    return $ \b -> readRef s >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            writeRef s $ Just (b, a)
            return a



instance MonadRefWriter IO where
    liftWriteRef = runRefWriterOfIO


---------------------------------

type Register n m = ReaderT (Ref m (MonadMonoid m, Command -> MonadMonoid n)) m

newtype Reg n a = Reg { unReg :: ReaderT (SLSt n () -> n ()) (Register n (SLSt n)) a } deriving (Monad, Applicative, Functor)

type SLSt (n :: * -> *) = IO

type Pure (n :: * -> *) = Reg IO
{-
mapReg :: (forall a . m a -> n a) -> Reg m a -> Reg n a
mapReg ff (Reg m) = Reg $ ReaderT $ \f -> ReaderT $ \r -> StateT $ \s -> 
    ff $ flip runStateT s $ flip runReaderT (iso undefined undefined `lensMap` r) $ runReaderT m $ undefined f

instance MonadTrans Reg where
    lift = Reg . lift . lift . lift
-}

instance MonadFix (Pure m) where
    mfix f = Reg $ mfix $ unReg . f

instance {- Monad m => -} MonadRefReader (Pure m) where

    type RefCore (Pure IO) = Lens_

    liftReadRef = Reg . lift . lift . liftReadRef

instance {-Monad n => -} ExtRef (Pure n) where
    extRef r l = Reg . lift . lift . extRef r l
    newRef = Reg . lift . lift . newRef
    memoRead = memoRead_
    memoWrite = memoWrite_
    future = future_

instance {-Monad n => -} MonadRefWriter (Pure n) where
    liftWriteRef = Reg . lift . lift . liftWriteRef

instance {-Monad n => -} EffRef (Pure n) where

    type EffectM (Pure IO) = IO

    newtype Modifier (Pure IO) a = RegW {unRegW :: Pure IO a} deriving (Monad, Applicative, Functor)

    liftEffectM = Reg . lift . lift

    liftModifier = RegW

    onChange_ r b0 c0 f = Reg $ ReaderT $ \ff ->
        toSend True id r b0 c0 $ \b b' c' -> liftM (\x -> evalRegister ff . x) $ evalRegister ff $ f b b' c'

    onChangeSimple r f = Reg $ ReaderT $ \ff ->
        toSend False id r undefined undefined $ \b _ _ -> return $ \_ -> evalRegister ff $ f b

    toReceive f g = Reg $ ReaderT $ \ff -> do
        tell' (mempty, MonadMonoid . g)
        writerstate <- ask
        return $ fmap (ff . flip runReaderT writerstate . evalRegister ff . unRegW) f

instance {- MonadFix m => -} MonadFix (Modifier (Pure m)) where
    mfix f = RegW $ mfix $ unRegW . f

instance {- Monad m => -} MonadRefWriter (Modifier (Pure m)) where
    liftWriteRef = RegW . liftWriteRef

instance {- Monad m => -} MonadRefReader (Modifier (Pure m)) where

    type RefCore (Modifier (Pure IO)) = Lens_

    liftReadRef = RegW . liftReadRef

instance {- Monad m => -} ExtRef (Modifier (Pure m)) where
    extRef r l = RegW . extRef r l
    newRef = RegW . newRef
    memoRead = memoRead_
    memoWrite = memoWrite_
    future = future_

evalRegister ff (Reg m) = runReaderT m ff

runPure :: (Monad m, m ~ IO) => (forall a . m (m a, a -> m ())) -> Pure m a -> m (a, m ())
runPure newChan (Reg m) = do
    (read, write) <- newChan
    (a, tick) <- do
        (a, r) <- runRefWriterT $ runReaderT m write
        (w, _) <- readRef r
        return (a, runMonadMonoid w)
    return $ (,) a $ forever $ do
        join $ read
        tick



toSend
    :: (Eq b, ExtRef m, MonadRefWriter m, Monad n)
    => Bool
    -> (n () -> m ())
    -> ReadRef m b
    -> b -> (b -> c)
    -> (b -> b -> c -> {-Either (Register m c)-} (Register n m (c -> Register n m c)))
    -> Register n m (ReadRef m c)
toSend memoize li rb b0 c0 fb = do
    let doit st = readRef st >>= runMonadMonoid . fst
        reg st msg = readRef st >>= li . runMonadMonoid . ($ msg) . snd

    memoref <- lift $ do
        b <- liftReadRef rb
        (c, st1) <- runRefWriterT $ fb b b0 $ c0 b0
        (val, st2) <- runRefWriterT $ c $ c0 b0
        doit st1
        doit st2
        newRef ((b, (c, val, st1, st2)), [])      -- memo table

    let act = MonadMonoid $ do
            b <- liftReadRef rb
            (last@(b', cc@(_, oldval, st1, st2)), memo) <- readRef memoref
            (_, _, st1, st2) <- if b' == b
              then
                return cc
              else do
                reg st1 Block
                reg st2 Kill
                (c, oldval', st1, _) <- case lookup b memo of
                  Nothing -> do
                    (c, st1) <- runRefWriterT $ fb b b' oldval
                    return (c, c0 b, st1, undefined)
                  Just cc'@(_, _, st1, _) -> do
                    reg st1 Unblock
                    return cc'
                (val, st2) <- runRefWriterT $ c oldval'
                let cc = (c, val, st1, st2)
                writeRef memoref ((b, cc), if memoize then filter ((/= b) . fst) (last:memo) else [])
                return cc
            doit st1
            doit st2

    tell' (act, mempty)
    return $ readRef $ (_1 . _2 . _2) `lensMap` memoref

----------------

-- Ref-based RefWriterT
type RefWriterT w m = ReaderT (Ref m w) m

runRefWriterT :: (ExtRef m, Monoid w) => RefWriterT w m a -> m (a, Ref m w)
runRefWriterT m = do
    r <- newRef mempty
    a <- runReaderT m r
    return (a, r)

tell' :: (Monoid w, ExtRef m, MonadRefWriter m) => w -> RefWriterT w m ()
tell' w = ReaderT $ \m -> readRef m >>= writeRef m . (`mappend` w)

-------------

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


