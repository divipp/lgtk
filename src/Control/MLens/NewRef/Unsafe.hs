{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module export only the @NewRef@ instance for @IO@ which does not fulfil the @NewRef@ laws in a multi-threaded environment.
module Control.MLens.NewRef.Unsafe
    () where

import Data.IORef

import qualified Data.MLens.Ref as Ref
import Control.MLens.NewRef
import Control.Monad.Restricted

instance Reference IORef where

    type RefMonad IORef = IO

    readRef = unsafeR . readIORef

    writeRef = writeIORef

    atomicModRef = atomicModifyIORef

-- | Note that this instance does not fulfil the @NewRef@ laws in a multi-threaded environment.
instance NewRef IO where

    type Ref IO = IORef

    liftInner = id

    newRef = unsafeC . newIORef

