{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Control.Monad.ExtRef.Pure
    ( Ext, IExt, runExt
    , Ext_, runExt_
    ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Category
import qualified Control.Arrow as Arrow
import Data.Sequence
import Data.Lens.Common
import Data.Foldable (toList)
import Prelude hiding ((.), id, splitAt, length)

import Unsafe.Coerce

import Control.Monad.ExtRef


data CC x = forall a . CC a (a -> x -> (a, x))

ap_ :: x -> CC x -> (x, CC x)
ap_ x (CC a set) = let
    (a', x') = set a x
    in (x', CC a' set)

unsafeData :: CC x -> a
unsafeData (CC x _) = unsafeCoerce x


newtype LSt = LSt (Seq (CC LSt))

initLSt :: LSt
initLSt = LSt empty

extend_
    :: (a -> LSt -> (a, LSt))
    -> (a -> LSt -> (a, LSt))
    -> a
    -> LSt
    -> ((LSt -> a, a -> LSt -> LSt), LSt)
extend_ rk kr a0 x0@(LSt x0_)
    = ((getM, setM), x0 ||> CC a0 kr)
  where
    getM = unsafeData . head' . snd . limit

    head' (x:_) = x
    head' _ = error "IMPOSSIBLE - extend"

    setM a x = case limit x of
        (zs, _ : ys) -> let
            (a', re) = rk a zs
            in foldl ((uncurry (||>) .) . ap_) (re ||> CC a' kr) ys

    LSt x ||> c = LSt (x |> c)

    limit (LSt y) = LSt Arrow.*** toList $ splitAt (length x0_) y

data MRef m a = MRef { readRef_ :: ReadPart m a, writeRef_ :: a -> m () }

instance HasReadPart m => Reference (MRef m) where

    type RefMonad (MRef m) = m

    readRef = readRef_

    writeRef = writeRef_

    l % MRef r w = MRef r' w'
     where
        r' = liftM (getL l) r

        w' b = do
            a <- runR r
            w $ setL l b a

    unitRef = MRef (return ()) (const $ return ())

    joinRef m = MRef (m >>= readRef_) (\a -> runR m >>= \r -> writeRef_ r a)

newtype Ext i m a = Ext { unExt :: StateT LSt m a }
    deriving (Functor, Monad, MonadWriter w)

instance MonadTrans (Ext i) where
    lift = Ext . lift

instance MonadIO m => MonadIO (Ext i m) where
    liftIO = lift . liftIO

mapExt :: Morph m n => Ext i m a -> Ext i n a
mapExt f = Ext . mapStateT f . unExt

type IExt i = Ext i Identity


newtype R' i a = R' (Reader LSt a) deriving (Functor, Monad)

instance Monad m => HasReadPart (Ext i m) where
    type ReadPart (Ext i m) = R' i
    runR (R' f) = Ext $ runR f


instance (Monad m) => ExtRef (Ext i m) where

    type Ref (Ext i m) = MRef (IExt i)

    liftWriteRef = mapExt (return . runIdentity)

    extRef r1 r2 a0 = Ext $ do
        a1 <- mapStateT (return . runIdentity) $ g a0
        (t,z) <- state $ extend_ (runState . f) (runState . g) a1
        return $ MRef (R' $ reader t) $ \a -> Ext $ modify $ z a
       where
        f a = unExt $ writeRef r1 (getL r2 a) >> return a
        g b = unExt $ runR $ liftM (flip (setL r2) b) $ readRef r1


-- | Basic running of the @(Ext i m)@ monad.
runExt :: Monad m => (forall i . Ext i m a) -> m a
runExt s = evalStateT (unExt s) initLSt

newtype Ext_ i m a = Ext_ (ReaderT (MorphD (State LSt) m) m a)
    deriving (Functor, Monad, MonadIO, MonadWriter w)

instance MonadTrans (Ext_ i) where
    lift = Ext_ . lift

instance (Monad m) => ExtRef (Ext_ i m) where

    type Ref (Ext_ i m) = MRef (IExt i)

    liftWriteRef (Ext m) = Ext_ $ do
        r <- ask
        lift $ runMorphD r m

    extRef r1 r2 a0 = liftWriteRef $ extRef r1 r2 a0


-- | Running of the @(Ext_ i m)@ monad.
runExt_ :: forall m a . Monad m
    => (forall a . a -> m (MorphD (State a) m))
    -> (forall i . Morph (Ext_ i m) m -> Ext_ i m a) -> m a
runExt_ nr f = do
    vx <- nr initLSt
    let unlift :: Morph (Ext_ i m) m
        unlift (Ext_ m) = runReaderT m vx
    unlift $ f unlift
  where
    swap (a, b) = (b, a)


