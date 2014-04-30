{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Data.LensRef.Pure
    ( Pure
    , runPure
    ) where

import Data.Monoid
import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***))
import Data.Sequence hiding (singleton, filter)
import Control.Lens hiding ((|>))
import Data.Foldable (toList)
import Prelude hiding (splitAt, length)

import Unsafe.Coerce

import Data.LensRef

----------------------

-- | @RefState (StateT s m) = Reader s@ 
instance Monad m => MonadRefReader (ReaderT s m) where
    newtype RefState (ReaderT s m) a = RSR { runRSR :: StateT s m a } deriving (Monad, Applicative, Functor, MonadReader s, MonadState s)
    liftRefStateReader m = RSR $ StateT $ \s -> liftM (\a -> (a,s)) $ runReaderT m s

----------------------

newtype Lens_ a b = Lens_ {unLens_ :: ALens' a b}

runLens_ :: Reader a (Lens_ a b) -> Lens' a b
runLens_ r f a = cloneLens (unLens_ $ runReader r a) f a

type LSt = Seq CC

data CC = forall a . CC (LSt -> a -> a) a

initLSt :: LSt
initLSt = empty

instance Reference (Lens_ LSt) where
    type RefReader (Lens_ LSt) = Reader LSt

    readRef = view . runLens_
    writeRef_ r a = runLens_ r .= a
    lensMap l r = return $ Lens_ $ runLens_ r . l
    unitRef = return $ Lens_ united

instance Monad m => ExtRef (StateT LSt m) where
    type RefCore (StateT LSt m) = Lens_ LSt

    liftReadRef m = state $ \s -> (runReader m s, s)

    extRef r r2 a0 = state extend
      where
        rk = set (runLens_ r) . (^. r2)
        kr = set r2 . (^. runLens_ r)

        extend x0 = (return $ Lens_ $ lens get set, x0 |> CC kr (kr x0 a0))
          where
            limit = (id *** toList) . splitAt (length x0)

            get = unsafeData . head . snd . limit

            set x a = foldl (\x -> (|>) x . ap_ x) (rk a zs |> CC kr a) ys where
                (zs, _ : ys) = limit x

        ap_ :: LSt -> CC -> CC
        ap_ x (CC f a) = CC f (f x a)

        unsafeData :: CC -> a
        unsafeData (CC _ a) = unsafeCoerce a

    memoRead = memoRead_

    memoWrite = memoWrite_

    future = future_

future_ :: ExtRefWrite m => (ReadRef m a -> m a) -> m a
future_ f = do
    s <- newRef $ error "can't see the future"
    a <- f $ readRef s
    writeRef s a
    return a

memoRead_ g = do
    s <- newRef Nothing
    return $ readRef' s >>= \x -> case x of
        Just a -> return a
        _ -> g >>= \a -> do
            writeRef s $ Just a
            return a

memoWrite_ g = do
    s <- newRef Nothing
    return $ \b -> readRef' s >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            writeRef s $ Just (b, a)
            return a

instance Monad m => ExtRefWrite (StateT LSt m) where
    liftWriteRef = state . runState . runRSR


---------------------------------


type Register n m = ReaderT (Ref m (MonadMonoid m, Command -> MonadMonoid n)) m

newtype Reg n a = Reg (ReaderT (SLSt n () -> n ()) (Register n (SLSt n)) a) deriving (Monad, Applicative, Functor)

type SLSt = StateT LSt

type Pure m = Reg m

mapReg :: (forall a . m a -> n a) -> Reg m a -> Reg n a
mapReg ff (Reg m) = Reg $ ReaderT $ \f -> ReaderT $ \r -> StateT $ \s -> 
    ff $ flip runStateT s $ flip runReaderT (iso undefined undefined `lensMap` r) $ runReaderT m $ undefined f

instance MonadTrans Reg where
    lift = Reg . lift . lift . lift


instance Monad n => ExtRef (Pure n) where

    type RefCore (Pure n) = Lens_ LSt

    liftReadRef = Reg . lift . lift . liftReadRef
    extRef r l = Reg . lift . lift . extRef r l
    newRef = Reg . lift . lift . newRef
    memoRead = memoRead_
    memoWrite = memoWrite_
    future = future_

instance Monad n => ExtRefWrite (Pure n) where
    liftWriteRef = Reg . lift . lift . liftWriteRef

instance Monad n => EffRef (Pure n) where

    type EffectM (Pure n) = n

    newtype Modifier (Pure n) a = RegW {unRegW :: Pure n a} deriving (Monad, Applicative, Functor)

    liftEffectM = lift

    liftModifier = RegW

    onChange_ r b0 c0 f = Reg $ ReaderT $ \ff ->
        toSend lift r b0 c0 $ \b b' c' -> liftM (\x -> evalRegister ff . x) $ evalRegister ff $ f b b' c'

    toReceive f g = Reg $ ReaderT $ \ff -> do
        tell' (mempty, MonadMonoid . g)
        writerstate <- ask
        return $ fmap (ff . flip runReaderT writerstate . evalRegister ff . unRegW) f

instance Monad m => ExtRefWrite (Modifier (Pure m)) where
    liftWriteRef = RegW . liftWriteRef

instance Monad m => ExtRef (Modifier (Pure m)) where

    type RefCore (Modifier (Pure m)) = Lens_ LSt

    liftReadRef = RegW . liftReadRef
    extRef r l = RegW . extRef r l
    newRef = RegW . newRef
    memoRead = memoRead_
    memoWrite = memoWrite_
    future = future_

evalRegister ff (Reg m) = runReaderT m ff

runPure :: Monad m => (forall a . m (m a, a -> m ())) -> Pure m a -> m (a, m ())
runPure newChan (Reg m) = do
    (read, write) <- newChan
    ((a, tick), s) <- flip runStateT initLSt $ do
        (a, r) <- runRefWriterT $ runReaderT m write
        (w, _) <- readRef' r
        return (a, runMonadMonoid w)
    return $ (,) a $ flip evalStateT s $ forever $ do
        join $ lift read
        tick



toSend
    :: (Eq b, ExtRefWrite m, Monad n)
    => (n () -> m ())
    -> ReadRef m b
    -> b -> (b -> c)
    -> (b -> b -> c -> {-Either (Register m c)-} (Register n m (c -> Register n m c)))
    -> Register n m (ReadRef m c)
toSend li rb b0 c0 fb = do
    let doit st = readRef' st >>= runMonadMonoid . fst
        reg st msg = readRef' st >>= li . runMonadMonoid . ($ msg) . snd

    memoref <- lift $ do
        b <- liftReadRef rb
        (c, st1) <- runRefWriterT $ fb b b0 $ c0 b0
        (val, st2) <- runRefWriterT $ c $ c0 b0
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
                    (c, st1) <- runRefWriterT $ fb b b' oldval
                    return (c, c0 b, st1, undefined)
                  Just cc'@(_, _, st1, _) -> do
                    reg st1 Unblock
                    return cc'
                (val, st2) <- runRefWriterT $ c oldval'
                let cc = (c, val, st1, st2)
                writeRef memoref ((b, cc), filter ((/= b) . fst) (last:memo))
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

tell' :: (Monoid w, ExtRefWrite m) => w -> RefWriterT w m ()
tell' w = ReaderT $ \m -> readRef' m >>= writeRef m . (`mappend` w)

-------------

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


