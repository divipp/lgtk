{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
Fast reference implementation for the @MonadRefCreator@ interface.

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
import Control.Monad.Trans.Control
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Data.LensRef
import Data.LensRef.Pure (memoRead_)

----------------------

newtype Wrap (m :: * -> *) a = Wrap {unWrap :: m a} deriving (Monad, Functor, Applicative, MonadFix)

instance MonadTrans Wrap where
    lift = Wrap

newtype instance RefWriterOf (Wrap m) a
    = RefWriterOfIO { runRefWriterOfIO :: Wrap m a }
        deriving (Monad, Applicative, Functor)

----------------------

data Lens__ (m :: * -> *) a = Lens_ 
    { readPart :: m a
    , writePart :: a -> m ()
    , register :: m () -> m ()
    }

type Lens_ m = Lens__ (Wrap m)

joinLens :: Monad m => Wrap m (Lens_ m a) -> Lens_ m a
joinLens m = Lens_
    { readPart = m >>= readPart
    , writePart = \a -> m >>= \r -> writePart r a
    , register = \e -> m >>= \r -> register r e
    }

instance MonadBaseControl IO m => RefClass (Lens_ m) where
    type RefReaderSimple (Lens_ m) = Wrap m

    readRefSimple = readPart . joinLens
    writeRefSimple m = RefWriterOfIO . writePart (joinLens m)
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

instance MonadBaseControl IO m => MonadRefReader (Wrap m) where

    type BaseRef (Wrap m) = Lens_ m

    liftRefReader = id

instance MonadBaseControl IO m => MonadRefReader (RefWriterOf (Wrap m)) where

    type BaseRef (RefWriterOf (Wrap m)) = Lens_ m

    liftRefReader = RefWriterOfIO

instance MonadBaseControl IO m => MonadRefWriter (RefWriterOf (Wrap m)) where
    liftRefWriter = id -- RefWriterOfIO . runRefWriterOfIO


wrap :: MonadBaseControl IO m => IO a -> Wrap m a
wrap m = Wrap $ liftBaseWith $ \_ -> m

instance MonadBaseControl IO m => MonadRefCreator (Wrap m) where

    extRef r r2 a0 = do
        Lens_ rb wb tb <- r
        b0 <- rb
        va <- wrap $ newMVar $ set r2 b0 a0
        reg <- wrap $ newMVar $ return ()
        status <- wrap $ newMVar True -- True: normal; False:
        tb $ do
            s <- wrap $ readMVar status
            when s $ do
                b <- rb
                wrap $ modifyMVar va $ \a -> return (set r2 b a, ())
                join $ wrap $ readMVar reg
        return $ do
            return $ Lens_
                { readPart = wrap $ readMVar va
                , writePart = \a -> do
                    _ <- wrap $ swapMVar va a
                    _ <- wrap $ swapMVar status False
                    wb $ a ^. r2
                    _ <- wrap $ swapMVar status True
                    join $ wrap $ readMVar reg
                , register = \m -> wrap $ modifyMVar reg $ \x -> return (x >> m, ())
                }

    newRef a0 = do
        va <- wrap $ newMVar a0
        reg <- wrap $ newMVar $ return ()
        return $ return $ Lens_
                { readPart = wrap $ readMVar va
                , writePart = \a -> do
                    _ <- wrap $ swapMVar va a
                    join $ wrap $ readMVar reg
                , register = \m -> wrap $ modifyMVar reg $ \x -> return (x >> m, ())
                }

instance MonadBaseControl IO m => MonadMemo (Wrap m) where
    memoRead = memoRead_
{-
    memoWrite = memoWrite_

    future = future_
-}


instance MonadBaseControl IO m => MonadRefWriter (Wrap m) where
    liftRefWriter = runRefWriterOfIO


---------------------------------

type Register n m = ReaderT (Ref m (MonadMonoid m, RegisteredCallbackCommand -> MonadMonoid n)) m

newtype Reg n a = Reg { unReg :: ReaderT (SLSt n () -> n ()) (Register n (SLSt n)) a } deriving (Monad, Applicative, Functor)

type SLSt (m :: * -> *) = m

type Pure m = Reg (Wrap m)
{-
mapReg :: (forall a . m a -> n a) -> Reg m a -> Reg n a
mapReg ff (Reg m) = Reg $ ReaderT $ \f -> ReaderT $ \r -> StateT $ \s -> 
    ff $ flip runStateT s $ flip runReaderT (iso undefined undefined `lensMap` r) $ runReaderT m $ undefined f

instance MonadTrans Reg where
    lift = Reg . lift . lift . lift
-}

instance MonadFix m => MonadFix (Pure m) where
    mfix f = Reg $ mfix $ unReg . f

instance MonadBaseControl IO m => MonadRefReader (Pure m) where

    type BaseRef (Pure m) = Lens_ m

    liftRefReader = Reg . lift . lift . liftRefReader

instance MonadBaseControl IO m => MonadRefCreator (Pure m) where
    extRef r l = Reg . lift . lift . extRef r l
    newRef = Reg . lift . lift . newRef

instance MonadBaseControl IO m => MonadMemo (Pure m) where
    memoRead = memoRead_
{-
    memoWrite = memoWrite_
    future = future_
-}
instance MonadBaseControl IO m => MonadRefWriter (Pure m) where
    liftRefWriter = Reg . lift . lift . liftRefWriter

instance MonadBaseControl IO m => MonadRegister (Pure m) where

    type EffectM (Pure m) = m

    newtype Modifier (Pure m) a = RegW {unRegW :: Pure m a} deriving (Monad, Applicative, Functor)

    liftEffectM = Reg . lift . lift . lift

    liftModifier = RegW

    onChangeAcc r b0 c0 f = Reg $ ReaderT $ \ff ->
        toSend True id r b0 c0 $ \b b' c' -> liftM (\x -> evalRegister ff . x) $ evalRegister ff $ f b b' c'

    onChangeSimple r f = Reg $ ReaderT $ \ff ->
        toSend False id r undefined undefined $ \b _ _ -> return $ \_ -> evalRegister ff $ f b

    registerCallback f g = Reg $ ReaderT $ \ff -> do
        tell' (mempty, MonadMonoid . Wrap . g)
        writerstate <- ask
        return $ fmap (unWrap . ff . flip runReaderT writerstate . evalRegister ff . unRegW) f

instance (MonadBaseControl IO m, MonadFix m) => MonadFix (Modifier (Pure m)) where
    mfix f = RegW $ mfix $ unRegW . f

instance MonadBaseControl IO m => MonadRefWriter (Modifier (Pure m)) where
    liftRefWriter = RegW . liftRefWriter

instance MonadBaseControl IO m => MonadRefReader (Modifier (Pure m)) where

    type BaseRef (Modifier (Pure m)) = Lens_ m

    liftRefReader = RegW . liftRefReader

instance MonadBaseControl IO m => MonadRefCreator (Modifier (Pure m)) where
    extRef r l = RegW . extRef r l
    newRef = RegW . newRef

instance MonadBaseControl IO m => MonadMemo (Modifier (Pure m)) where
    memoRead = memoRead_
{-
    memoWrite = memoWrite_
    future = future_
-}
evalRegister ff (Reg m) = runReaderT m ff

runPure :: MonadBaseControl IO m => (forall a . m (m a, a -> m ())) -> Pure m a -> m (a, m ())
runPure newChan (Reg m) = unWrap $ do
    (read, write) <- Wrap newChan
    (a, tick) <- do
        (a, r) <- runRefWriterT $ runReaderT m $ Wrap . write
        (w, _) <- readRef r
        return (a, runMonadMonoid w)
    return $ (,) a $ unWrap $ forever $ do
        join $ Wrap read
        tick



toSend
    :: (Eq b, MonadRefCreator m, MonadRefWriter m, Monad n)
    => Bool
    -> (n () -> m ())
    -> RefReader m b
    -> b -> (b -> c)
    -> (b -> b -> c -> {-Either (Register m c)-} (Register n m (c -> Register n m c)))
    -> Register n m (RefReader m c)
toSend memoize li rb b0 c0 fb = do
    let doit st = readRef st >>= runMonadMonoid . fst
        reg st msg = readRef st >>= li . runMonadMonoid . ($ msg) . snd

    memoref <- lift $ do
        b <- liftRefReader rb
        (c, st1) <- runRefWriterT $ fb b b0 $ c0 b0
        (val, st2) <- runRefWriterT $ c $ c0 b0
        doit st1
        doit st2
        newRef ((b, (c, val, st1, st2)), [])      -- memo table

    let act = MonadMonoid $ do
            b <- liftRefReader rb
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

runRefWriterT :: (MonadRefCreator m, Monoid w) => RefWriterT w m a -> m (a, Ref m w)
runRefWriterT m = do
    r <- newRef mempty
    a <- runReaderT m r
    return (a, r)

tell' :: (Monoid w, MonadRefCreator m, MonadRefWriter m) => w -> RefWriterT w m ()
tell' w = ReaderT $ \m -> readRef m >>= writeRef m . (`mappend` w)

-------------

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


