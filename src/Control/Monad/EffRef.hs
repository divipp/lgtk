{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.EffRef
    ( EffRef
    , EffIORef
    , fileRef
    ) where

import Control.Monad
import Control.Monad.Trans
import System.Directory
import Prelude hiding ((.), id)

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef


type EffRef m = (ExtRef m, MonadRegister m, Inner m ~ Inner' m)

type EffIORef m = (EffRef m, Inn m ~ IO)

fileRef :: (EffIORef m) => FilePath -> C m (Ref m (Maybe String))
fileRef f = unsafeC $ do
        ms <- liftInn $ liftIO r
        ref <- runC $ newRef ms
        -- addWEffect (writeRef ref) $ \cb -> TODO
        addCEffect (readRef ref) $ liftIO . w
        return ref
     where
        r = do
            b <- doesFileExist f
            if b then do
                xs <- readFile f
                length xs `seq` return (Just xs)
             else return Nothing

        w = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)

