{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Monad.Restricted where

import Data.Monoid
--import Control.Monad.Layer hiding (MonadTrans, lift)
--import qualified Control.Monad.Layer as L
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Trans.Identity
import qualified System.Environment as Env
import System.IO.Error (catchIOError, isDoesNotExistError)

{- |
Monad morphism. Think as @m@ is a submonad of @n@.
-}
type Morph m n = forall a . m a -> n a

{- |
The @MorphD@ type is needed only to avoid impredicative types.
We use @MorphD@ instead of @Morph@ when the morphism is stored inside a data structure.
-}
newtype MorphD m n = MorphD { runMorphD :: Morph m n }

------------------

newtype Ext n m a = Ext { unExt :: ReaderT (MorphD n m) m a }
    deriving (Monad, MonadIO, Functor, Applicative)

instance MonadTrans (Ext n) where
    lift = Ext . lift

deriving instance (MonadBase b m) => MonadBase b (Ext n m)
{-
instance MonadTransControl (Ext n) where
    data StT (Ext n) a = StTExt { unStTExt :: a }
--    liftWith     = defaultLiftWith Ext
--    restoreT     = defaultRestoreT   unExt

instance (MonadBaseControl b m) => MonadBaseControl b (Ext n m) where
    data StM (Ext n m) a = StMExt { unStMExt :: ComposeSt (Ext n) m a }
    liftBaseWith = defaultLiftBaseWith StMExt
    restoreM     = defaultRestoreM   unStMExt
-}

instance (MonadBase m m) => MonadBaseControl m (Ext n m) where
    data StM (Ext n m) a = StMExt { unStMExt :: a }
    liftBaseWith f = Ext $ do
        r <- ask
        lift $ f $ liftM StMExt . flip runReaderT r . unExt
    restoreM = return . unStMExt

lift' :: Monad m => n a -> Ext n m a
lift' m = Ext $ do
    r <- ask
    lift $ runMorphD r m

unlift :: (MonadBase m m) => ((Ext n m a -> m a) -> m b) -> Ext n m b
unlift f = liftBaseWith $ \m -> f (liftM (unStMExt) . m)

runExt :: MorphD n m -> Ext n m a -> m a
runExt v (Ext m) = runReaderT m v

------------------

--type MonadIO' m = (MonadIO m, MonadBaseControl IO m)

unliftIO' :: MonadBaseControl n m => ((m () -> n ()) -> m a) -> m a
unliftIO' f = liftBaseWith (\m -> m $ f $ void . m) >>= restoreM

unliftIO :: MonadBaseControl n m => ((m () -> n ()) -> n a) -> m a
unliftIO f = liftBaseWith (\m -> f $ void . m)

-------------------

-- | Type class for effectless, synchronous @IO@ actions.
class Monad m => SafeIO m where

    -- | The program's command line arguments (not including the program name). 
    getArgs     :: m [String]

    -- | The name of the program as it was invoked.
    getProgName :: m String

    -- | @(lookupEnv var)@ returns the value of the environment variable @var@.
    lookupEnv   :: String -> m (Maybe String)

-- | This instance is used in the implementation, the end users do not need it.
instance SafeIO IO where

    getArgs     = Env.getArgs
    getProgName = Env.getProgName
--    lookupEnv   = Env.lookupEnv -- does not work with Haskell Platform 2013.2.0.0
    lookupEnv v = catchIOError (liftM Just $ Env.getEnv v) $ \e ->
        if isDoesNotExistError e then return Nothing else ioError e

-- | This instance is used in the implementation, the end users do not need it.
instance SafeIO m => SafeIO (Ext n m) where

    getArgs     = lift getArgs
    getProgName = lift getProgName
    lookupEnv   = lift . lookupEnv

-- | This instance is used in the implementation, the end users do not need it.
instance SafeIO m => SafeIO (IdentityT m) where

    getArgs     = lift getArgs
    getProgName = lift getProgName
    lookupEnv   = lift . lookupEnv

-- | This instance is used in the implementation, the end users do not need it.
instance (SafeIO m, Monoid w) => SafeIO (RWST r w s m) where

    getArgs     = lift getArgs
    getProgName = lift getProgName
    lookupEnv   = lift . lookupEnv

-------------------

class (Monad m) => NewRef m where
    newRef' :: a -> m (MorphD (StateT a m) m)

instance NewRef IO where
    newRef' x = do
        vx <- liftIO $ newMVar x
        return $ MorphD $ \m -> modifyMVar vx $ liftM swap . runStateT m
      where
        swap (a, b) = (b, a)

newRef'_ :: (MonadIO m, MonadIO n) => String -> a -> m (MorphD (StateT a n) n)
newRef'_ msg x = do
        vx <- liftIO $ newMVar x
        return $ MorphD $ \m -> do
            st <- liftIO $ takeMVar vx
            (a, st') <- runStateT m st
            liftIO $ putMVar vx st'
            return a

instance (MonadBase m m, NewRef m) => NewRef (Ext n m) where
    newRef' = liftM (\m -> MorphD $ \k -> unlift $ runMorphD m . flip mapStateT k) . lift . newRef'

-------------------

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


