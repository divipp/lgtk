{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{- |
IORef-based implementation for the @ExtRef@ interface.

The implementation uses @unsafePerformIO@ internally, but its effect cannot escape.
-}
module Control.MLens.ExtRef.IORef
    ( Ext, runExt, runExt_
    ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Category
import Prelude hiding ((.), id, splitAt, length)

import System.IO.Unsafe

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.NewRef.Unsafe ()


extRef_ :: NewRef m => Ref m b -> MLens m a b -> a -> m (Ref m a)
extRef_ r1 r2 a0 = do
    inner <- readRef r1
    a' <- setL r2 inner a0
    store <- newRef a'
    return $ Ref $ MLens $ \unit -> do
        a <- readRef store
        inner <- readRef r1
        a' <- setL r2 inner a
--        writeRef store a'
        return $ (,) a' $ \a -> do
            x <- getL r2 a
            writeRef r1 x
            writeRef store a
            return unit

instance ExtRef IO where
    extRef = extRef_

instance (Monad m, NewRef m, Monoid w) => ExtRef (WriterT w m) where
    extRef = extRef_

newtype Ext i m a = Ext { unExt :: m a } deriving (Functor, Monad, MonadWriter w)

instance MonadTrans (Ext i) where
    lift = Ext

unsafeLiftIO :: Monad m =>  IO a -> Ext i m a
unsafeLiftIO m = do
    let a = unsafePerformIO m
    a `seq` return a

instance Monad m => NewRef (Ext i m) where
    newRef = liftM (mapRef unsafeLiftIO) . unsafeLiftIO . newRef

instance Monad m => ExtRef (Ext i m) where
    extRef = extRef_

-- | Basic running of the @(Ext i m)@ monad.
runExt :: Monad m => (forall i . Ext i m a) -> m a
runExt m = unExt m

{- |
Advanced running of the @(Ext i m)@ monad.

@Functor@ in contexts would not be needed if it were a superclass of @Monad@.
-}
runExt_
    :: forall c m . (Functor m, NewRef m)
    => (forall n . (Monad n, Functor n) => Morph m n -> Morph n m -> c n -> c m)
    -> (forall i . c (Ext i m)) -> m (c m)
runExt_ mapI int
    = return $ mapI lift unExt int

