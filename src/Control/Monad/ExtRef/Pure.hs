{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Control.Monad.ExtRef.Pure where

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

import Control.Monad.ExtRef

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
    writeRef r a = runLens_ r .= a
    lensMap l r = return $ Lens_ $ runLens_ r . l
    unitRef = return $ Lens_ united

instance Monad m => ExtRef (StateT LSt m) where
    type RefCore (StateT LSt m) = Lens_ LSt

    liftWriteRef = state . runState . runRSR

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

---------------------------------


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
        tell' (mempty, MonadMonoid . g)
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
        (c, st1) <- runRefWriterT' $ fb b b0 $ c0 b0
        (val, st2) <- runRefWriterT' $ c $ c0 b0
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
                    (c, st1) <- runRefWriterT' $ fb b b' oldval
                    return (c, c0 b, st1, undefined)
                  Just cc'@(_, _, st1, _) -> do
                    reg st1 Unblock
                    return cc'
                (val, st2) <- runRefWriterT' $ c oldval'
                let cc = (c, val, st1, st2)
                liftWriteRef $ writeRef memoref ((b, cc), filter ((/= b) . fst) (last:memo))
                return cc
            doit st1
            doit st2

    tell' (act, mempty)
    return $ readRef $ (_1 . _2 . _2) `lensMap` memoref

----------------

-- Ref-based RefWriterT
type RefWriterT w m = ReaderT (Ref m w) m

runRefWriterT' :: (ExtRef m, Monoid w) => RefWriterT w m a -> m (a, Ref m w)
runRefWriterT' m = do
    r <- newRef mempty
    a <- runReaderT m r
    return (a, r)

runRefWriterT :: (ExtRef m, Monoid w) => RefWriterT w m a -> m (a, w)
runRefWriterT m = do
    (a, r) <- runRefWriterT' m
    w <- readRef' r
    return (a, w)

tell' :: (Monoid w, ExtRef m) => w -> RefWriterT w m ()
tell' w = ReaderT $ \m -> readRef' m >>= liftWriteRef . writeRef m . (`mappend` w)

-------------

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


