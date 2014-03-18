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

--import Control.Monad.Base
--import Control.Monad.Trans.Control
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Operational
import Control.Arrow ((***))
import Data.Sequence hiding (singleton)
import Data.Lens.Common
import Data.Foldable (toList)
import Prelude hiding (splitAt, length)

import Unsafe.Coerce

import Control.Monad.ExtRef

----------------- synthetic data types and instances

type SyntRefReader x = Program (RefReaderI x)
data RefReaderI x a where
    SyntReadRef :: SyntRef x a -> RefReaderI x a

type SyntRefState x = Program (RefStateI x)
data RefStateI x a where
    SyntLiftRefReader :: SyntRefReader x a -> RefStateI x a
    SyntWriteRef :: SyntRef x a -> a -> RefStateI x ()

instance MonadRefState (SyntRefState x) where
    type RefStateReader (SyntRefState x) = SyntRefReader x
    liftRefStateReader = singleton . SyntLiftRefReader

data SyntRef x a where
    SyntUnitRef :: SyntRef x ()
    SyntLensMap :: Lens' a b -> SyntRef x a -> SyntRef x b
    SyntJoinRef :: SyntRefReader x (SyntRef x a) -> SyntRef x a
    SyntCreatedRef :: x a -> SyntRef x a

instance Reference (SyntRef x) where
    type RefState (SyntRef x) = SyntRefState x
    readRef = singleton . SyntReadRef
    writeRef r = singleton . SyntWriteRef r
    lensMap = SyntLensMap
    joinRef = SyntJoinRef
    unitRef = SyntUnitRef

type SyntExtRef x = Program (ExtRefI x)
data ExtRefI x a where
    SyntLiftRefState :: SyntRefState x a -> ExtRefI x a
    SyntExtRef :: SyntRef x b -> Lens' a b -> a -> ExtRefI x (SyntRef x a)
--    SyntNewRef :: a -> ExtRefI x (SyntRef x a)

instance ExtRef (SyntExtRef x) where
    type Ref (SyntExtRef x) = SyntRef x
    liftWriteRef w = singleton $ SyntLiftRefState w
    extRef r l = singleton . SyntExtRef r l
--    newRef = singleton . SyntNewRef


----------------------

newtype Lens_ a b = Lens_ {unLens_ :: Lens' a b}

type LSt = Seq CC

data CC = forall a . CC (LSt -> a -> a) a

initLSt :: LSt
initLSt = empty


runSyntRefReader :: SyntRefReader (Lens_ x) a -> Reader x a
runSyntRefReader = interpretWithMonad eval where
    eval (SyntReadRef r) = reader $ getL $ unLens_ $ runSyntRef r

runSyntRefState :: SyntRefState (Lens_ x) a -> State x a
runSyntRefState = interpretWithMonad eval where
    eval (SyntLiftRefReader r) = liftRefStateReader $ runSyntRefReader r
    eval (SyntWriteRef r a) = modify $ setL (unLens_ $ runSyntRef r) a

runSyntRef :: SyntRef (Lens_ x) a -> Lens_ x a
runSyntRef SyntUnitRef = Lens_ $ lens (const ()) $ const . id
runSyntRef (SyntLensMap l r) = Lens_ $ (unLens_ $ runSyntRef r) . l
runSyntRef (SyntJoinRef m) = (\f -> Lens_ $ \g s -> unLens_ (f s) g s) $ runReader $ liftM runSyntRef $ runSyntRefReader m
runSyntRef (SyntCreatedRef l) = l

runExtRef :: Monad m => SyntExtRef (Lens_ LSt) a -> StateT LSt m a
runExtRef = interpretWithMonad eval where
    eval (SyntLiftRefState w) = mapStateT (return . runIdentity) $ runSyntRefState w
    eval (SyntExtRef r r2 a0) = state extend
     where
        r1 = runSyntRef r

        rk = setL (unLens_ r1) . getL r2
        kr = setL r2 . getL (unLens_ r1)

        extend x0 = (SyntCreatedRef $ Lens_ $ lens get set, x0 |> CC kr (kr x0 a0))
          where
            limit = (id *** toList) . splitAt (length x0)

            get = unsafeData . head . snd . limit

            set x a = foldl (\x -> (|>) x . ap_ x) (rk a zs |> CC kr a) ys where
                (zs, _ : ys) = limit x

        ap_ :: LSt -> CC -> CC
        ap_ x (CC f a) = CC f (f x a)

        unsafeData :: CC -> a
        unsafeData (CC _ a) = unsafeCoerce a

--    eval (SyntNewRef a) = newRef a

