{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module export only the @NewRef@ instance for @IO@ which does not fulfil the @NewRef@ laws in a multi-threaded environment.
module Control.MLens.NewRef.Unsafe
    () where

import Data.IORef

import Data.MLens.Ref
import Control.MLens.NewRef
import Control.Monad.Restricted

-- | Note that this instance does not fulfil the @NewRef@ laws in a multi-threaded environment.
instance NewRef IO where
    type Inner IO = IO

    liftInner m = m

    newRef x = unsafeC $ do
        r <- newIORef x
        return $ Ref (unsafeR $ readIORef r) (writeIORef r)


