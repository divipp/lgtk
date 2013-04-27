{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Control.MLens.ExtRef.Pure
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

import Control.MLens.ExtRef
import Control.Monad.Restricted


data CC x = forall a . CC a (a -> x -> (a, x))

ap_ :: x -> CC x -> (x, CC x)
ap_ x (CC a set) = let
    (a', x') = set a x
    in (x', CC a' set)

unsafeData :: CC x -> a
unsafeData (CC x _) = unsafeCoerce x


newtype ST = ST (Seq (CC ST))

initST :: ST
initST = ST empty

extend_
    :: (a -> ST -> (a, ST))
    -> (a -> ST -> (a, ST))
    -> a
    -> ST
    -> ((ST -> a, a -> ST -> ST), ST)
extend_ rk kr a0 x0@(ST x0_)
    = ((getM, setM), x0 ||> CC a0 kr)
  where
    getM = unsafeData . head' . snd . limit

    head' (x:_) = x
    head' _ = error "IMPOSSIBLE - extend"

    setM a x = case limit x of
        (zs, _ : ys) -> let
            (a', re) = rk a zs
            in foldl (((uncurry (||>)) .) . ap_) (re ||> CC a' kr) ys

    ST x ||> c = ST (x |> c)

    limit (ST y) = ST Arrow.*** toList $ splitAt (length x0_) y

newtype Ext i m a = Ext { unExt :: StateT ST m a }
    deriving (Functor, Monad, MonadWriter w)

instance MonadTrans (Ext i) where
    lift = Ext . lift

instance MonadIO m => MonadIO (Ext i m) where
    liftIO = lift . liftIO

mapExt :: Morph m n => Ext i m a -> Ext i n a
mapExt f = Ext . mapStateT f . unExt

type IExt i = Ext i Identity

extRef_ :: Monad m => Ref (IExt i) x -> Lens a x -> a -> C (Ext i m) (Ref (IExt i) a)
extRef_ r1 r2 a0 = unsafeC $ Ext $ do
    a1 <- mapStateT (return . runIdentity) $ g a0
    (t,z) <- state $ extend_ (runState . f) (runState . g) a1
    return $ Ref (unsafeR $ Ext (gets t)) $ \a -> Ext $ modify $ z a
   where
    f a = unExt $ writeRef r1 (getL r2 a) >> return a
    g b = unExt $ runR $ liftM (flip (setL r2) b) $ readRef r1

instance (Monad m) => NewRef (Ext i m) where
    type Inner (Ext i m) = IExt i

    liftInner = mapExt (return . runIdentity)

    newRef = extRef_ unitRef $ lens (const ()) (const id)

instance (Monad m) => ExtRef (Ext i m) where
    extRef = extRef_

-- | Basic running of the @(Ext i m)@ monad.
runExt :: Monad m => (forall i . Ext i m a) -> m a
runExt s = evalStateT (unExt s) initST


newtype Ext_ i m a = Ext_ { unExt_ :: ReaderT (IRef m ST) m a }
    deriving (Functor, Monad, MonadWriter w)

instance MonadTrans (Ext_ i) where
    lift = Ext_ . lift

liftInner_ :: NewRef m => IExt i a -> Ext_ i m a
liftInner_ (Ext m) = Ext_ $ do
    r <- ask
    lift $ liftInner $ do
        s <- runR $ readRef r
        let (a, s') = runState m s
        writeRef r s'
        return a

extRef_' :: NewRef m => Ref (IExt i) x -> Lens a x -> a -> C (Ext_ i m) (Ref (IExt i) a)
extRef_' r1 r2 a0 = mapC liftInner_ $ extRef_ r1 r2 a0

instance (NewRef m) => NewRef (Ext_ i m) where

    type Inner (Ext_ i m) = IExt i

    liftInner = liftInner_

    newRef = extRef_' unitRef $ lens (const ()) (const id)

instance (NewRef m) => ExtRef (Ext_ i m) where

    extRef = extRef_'

-- | Running of the @(Ext_ i m)@ monad.
runExt_ :: forall m a . NewRef m => (forall i . Morph (Ext_ i m) m -> Ext_ i m a) -> m a
runExt_ f = do
    vx <- runC $ newRef initST
    let unlift :: Morph (Ext_ i m) m
        unlift (Ext_ m) = runReaderT m vx
    unlift $ f unlift

instance MonadIO m => MonadIO (Ext_ i m) where

    liftIO m = Ext_ $ liftIO m

