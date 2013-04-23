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
    ( Ext, mapExt, IExt, runExt, runExt_
    ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Category
import qualified Control.Arrow as Arrow
import Data.Sequence
import Data.Lens.Common
import Data.Foldable (toList)
import Prelude hiding ((.), id, splitAt, length)

import Unsafe.Coerce

import Data.MLens.Ref
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
extend_ rk kr a0 x0
    = ((getM, setM), x0 ||> CC a0 kr)
  where
    getM = unsafeData . head . snd . limit x0

    setM a x = case limit x0 x of
        (zs, _ : ys) -> let
            (a', re) = rk a zs
            in foldl (((uncurry (||>)) .) . ap_) (re ||> CC a' kr) ys

    ST x ||> c = ST (x |> c)

    limit (ST x) (ST y) = ST Arrow.*** toList $ splitAt (length x) y



newtype Ext i m a = Ext { unExt :: StateT ST m a }
    deriving (Functor, Monad, MonadWriter w)

instance MonadTrans (Ext i) where
    lift = Ext . lift

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

{- |
Advanced running of the @(Ext i m)@ monad.

@Functor@ in contexts would not be needed if it were a superclass of @Monad@.
-}
runExt_
    :: forall c m . (Functor m, NewRef m)
    => (forall n . (Monad n, Functor n) => Morph m n -> Morph n m -> c n -> c m)
    -> (forall i . c (Ext i m)) -> m (c m)
runExt_ mapI int = do
    vx <- runC $ newRef initST
    let unlift :: Morph (Ext i m) m
        unlift f = do
            x <- liftInner $ runR $ readRef vx
            (b, x) <- runStateT (unExt f) x
            liftInner $ writeRef vx x
            return b
    return $ mapI lift unlift int

