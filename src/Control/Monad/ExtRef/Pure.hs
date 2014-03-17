{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Control.Monad.ExtRef.Pure where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Arrow ((***))
import Data.Sequence
import Data.Lens.Common
import Data.Foldable (toList)
import Prelude hiding (splitAt, length)

import Unsafe.Coerce
import System.IO.Unsafe

import Control.Monad.Restricted
import Control.Monad.ExtRef
import Control.Monad.Operational

newtype Lens_ a b = Lens_ {unLens_ :: Lens' a b}

instance Reference (Lens_ a) where

    type RefState (Lens_ a) = State a

    readRef (Lens_ r) = reader $ getL r

    writeRef (Lens_ r) = modify . setL r

    lensMap l (Lens_ r) = Lens_ $ r . l

    unitRef = Lens_ $ lens (const ()) (flip $ const id)

    joinRef m = (\f -> Lens_ $ \g s -> unLens_ (f s) g s) $ runReader m


type LSt = Seq CC

initLSt :: LSt
initLSt = empty

data CC = forall a . CC (LSt -> a -> a) a

ap_ :: LSt -> CC -> CC
ap_ x (CC f a) = CC f (f x a)

unsafeData :: CC -> a
unsafeData (CC _ a) = unsafeCoerce a


instance Monad m => ExtRef (StateT LSt m) where

    type Ref (StateT LSt m) = Lens_ LSt

    liftWriteRef = mapStateT (return . runIdentity)

    extRef (Lens_ r1) r2 a0 = state extend  where

        rk = setL r1 . getL r2
        kr = setL r2 . getL r1

        extend x0 = (Lens_ $ lens get set, x0 |> CC kr (kr x0 a0))
          where
            limit = (id *** toList) . splitAt (length x0)

            get = unsafeData . head . snd . limit

            set x a = foldl (\x -> (|>) x . ap_ x) (rk a zs |> CC kr a) ys where
                (zs, _ : ys) = limit x

---------------

type X = Lens_ LSt

runSyntRefReader :: SyntRefReader (Lens_ x) a -> Reader x a
runSyntRefReader = interpretWithMonad eval where
--    eval :: RefReaderI (Lens_ x) a -> Reader x a
    eval (SyntReadRef r) = readRef $ runSyntRef r

runSyntRefState :: SyntRefState (Lens_ x) a -> State x a
runSyntRefState = interpretWithMonad eval where
--    eval :: RefStateI (Lens_ x) a -> State x a
    eval (SyntLiftRefReader r) = liftRefStateReader $ runSyntRefReader r
    eval (SyntWriteRef r a) = writeRef (runSyntRef r) a

runSyntRef :: SyntRef (Lens_ x) a -> Lens_ x a
runSyntRef SyntUnitRef = unitRef
runSyntRef (SyntLensMap l r) = lensMap l $ runSyntRef r
runSyntRef (SyntJoinRef m) = joinRef $ liftM runSyntRef $ runSyntRefReader m
runSyntRef (SyntCreatedRef l) = l

runExtRef'' :: Monad m => SyntExtRef (Lens_ LSt) a -> StateT LSt m a
runExtRef'' = interpretWithMonad eval where
--    eval :: (Monad m, ExtRef (StateT x m), Ref (StateT x m) ~ Lens_ x) => ExtRefI (Lens_ x) a -> StateT x m a
    eval (SyntLiftRefState w) = liftWriteRef $ runSyntRefState w
    eval (SyntExtRef r l a) = liftM SyntCreatedRef $ extRef (runSyntRef r) l a
    eval (SyntNewRef a) = liftM SyntCreatedRef $ newRef a

---------------

instance (ExtRef n, Monad m) => ExtRef (Ext n m) where
    type Ref (Ext n m) = Ref n
    liftWriteRef = lift' . liftWriteRef
    extRef r1 r2 = lift' . extRef r1 r2


-- | Basic running of the @ExtRef@ monad.
runExtRef :: Monad m => (forall t . (MonadTrans t, ExtRef (t m)) => t m a) -> m a
runExtRef s = evalStateT s initLSt


instance SafeIO (Reader (Seq CC)) where

    getArgs     = runSafeIO getArgs
    getProgName = runSafeIO getProgName
    lookupEnv   = runSafeIO . lookupEnv

runSafeIO :: Monad m => IO a -> m a
runSafeIO = return . unsafePerformIO

--instance (MonadBaseControl IO m) => SafeIO m where

-- | Advanced running of the @ExtRef@ monad.
runExtRef_
    :: forall m a . (MonadBase m m, NewRef m)
    => (forall t . (MonadTrans t, ExtRef (t m), NewRef (t m), MonadIO (t IO), MonadBaseControl IO (t IO), SafeIO (ReadRef (t IO)), SafeIO (t IO)) => t m a)
    -> m a
--    -> (Morph (Ext (State LSt) m) m -> Ext (State LSt) m a) -> m a
runExtRef_ f = newRef' initLSt >>= flip runExt f





