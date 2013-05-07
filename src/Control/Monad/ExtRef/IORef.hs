{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{- |
IORef-based implementation for the @ExtRef@ interface.

The implementation uses @unsafePerformIO@ internally, but its effect cannot escape.
-}
module Control.Monad.ExtRef.IORef
    ( Ext, runExt, runExt_
    ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Category
import Control.Exception (evaluate)
import Data.Lens.Common
import Prelude hiding ((.), id, splitAt, length)

import System.IO.Unsafe

import Control.Monad.ExtRef
import Control.Monad.NewRef.Unsafe ()
import Control.Monad.Restricted


extRef_ :: NewRef m => IRef m b -> Lens a b -> a -> C m (IRef m a)
extRef_ r1 r2 a0 = do
    inner <- mapC liftInner $ rToC $ readRef r1
    let a' = setL r2 inner a0
    store <- newRef a'
    let r = do 
            a <- readRef store
            inner <- readRef r1
            let a' = setL r2 inner a
            return a'
        w a = do
            let x = getL r2 a
            writeRef r1 x
            writeRef store a
    return $ Ref r w

instance ExtRef IO where
    extRef = extRef_

newtype Ext i m a = Ext { unExt :: m a } deriving (Functor, Monad, MonadWriter w)

instance MonadTrans (Ext i) where
    lift = Ext

unsafeLiftIO :: Monad m =>  IO a -> Ext i m a
unsafeLiftIO m = do
    evaluate $ unsafePerformIO m

instance Monad m => NewRef (Ext i m) where
    type Inner (Ext i m) = Ext i m

    liftInner m = m

    newRef = liftM (mapRef unsafeLiftIO) . mapC unsafeLiftIO . newRef

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

