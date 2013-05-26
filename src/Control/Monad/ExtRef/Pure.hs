{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Control.Monad.ExtRef.Pure
    ( runExtRef
    , runExtRef_
    ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Category
import qualified Control.Arrow as Arrow
import Data.Sequence
import Data.Lens.Common
import Data.Foldable (toList)
import Prelude hiding ((.), id, splitAt, length)

import Unsafe.Coerce

import Control.Monad.Restricted
import Control.Monad.ExtRef


instance Reference (Lens a) where

    type RefMonad (Lens a) = State a

    readRef = reader . getL

    writeRef r = modify . setL r

    lensMap = (.)

    unitRef = lens (const ()) (const id)

    joinRef = Lens . join . (runLens .) . runReader


type LSt = Seq CC

initLSt :: LSt
initLSt = empty

data CC = forall a . CC (LSt -> a -> a) a

ap_ :: LSt -> CC -> CC
ap_ x (CC f a) = CC f (f x a)

unsafeData :: CC -> a
unsafeData (CC _ a) = unsafeCoerce a


instance Monad m => ExtRef (StateT LSt m) where

    type Ref (StateT LSt m) = Lens LSt

    liftWriteRef = mapStateT (return . runIdentity)

    extRef r1 r2 a0 = state extend  where

        rk = setL r1 . getL r2
        kr = setL r2 . getL r1

        extend x0 = (lens get set, x0 |> CC kr (kr x0 a0))
          where
            limit = (id Arrow.*** toList) . splitAt (length x0)

            get = unsafeData . head . snd . limit

            set a x = foldl (\x -> (|>) x . ap_ x) (rk a zs |> CC kr a) ys where
                (zs, _ : ys) = limit x


instance (ExtRef n, Monad m) => ExtRef (Ext n m) where
    type Ref (Ext n m) = Ref n
    liftWriteRef = lift' . liftWriteRef
    extRef r1 r2 = lift' . extRef r1 r2


-- | Basic running of the @ExtRef@ monad.
runExtRef :: Monad m => (forall t . (MonadTrans t, ExtRef (t m)) => t m a) -> m a
runExtRef s = evalStateT s initLSt

-- | Advanced running of the @ExtRef@ monad.
runExtRef_
    :: forall m a . NewRef m
    => (forall t . (MonadTrans t, ExtRef (t m), NewRef (t m), MonadIO' (t IO)) => t m a)
    -> m a
--    -> (Morph (Ext (State LSt) m) m -> Ext (State LSt) m a) -> m a
runExtRef_ f = newRef' initLSt >>= flip runExt f





