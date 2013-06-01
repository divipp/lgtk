{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted
    ( -- * Auxiliary definitions
      Morph
    , MorphD (..)
    , Ext (..), lift', runExt
    , HasReadPart (..)
    , MonadIO' (..)
    , SafeIO (..)
    , NewRef (..)
    , MonadMonoid (..)
    ) where

import Data.Monoid
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

-- | @m@ has a submonad @(ReadPart m)@ which is isomorphic to 'Reader'.
class (Monad m, Monad (ReadPart m)) => HasReadPart m where

    {- | Law: @(ReadPart m)@  ===  @('Reader' x)@ for some @x@.

    Alternative laws which ensures this isomorphism (@r :: (ReadPart m a)@ is arbitrary):

     *  @(r >> return ())@ === @return ()@

     *  @liftM2 (,) r r@ === @liftM (\a -> (a, a)) r@

    See also <http://stackoverflow.com/questions/16123588/what-is-this-special-functor-structure-called>
    -}
    type ReadPart m :: * -> *

    -- | @(ReadPart m)@ is a submonad of @m@
    liftReadPart :: Morph (ReadPart m) m

-- | @ReadPart (StateT s m) = Reader s@ 
instance Monad m => HasReadPart (StateT s m) where
    type ReadPart (StateT s m) = Reader s
    liftReadPart = gets . runReader


newtype Ext n m a = Ext { unExt :: ReaderT (MorphD n m) m a }
    deriving (Monad, MonadIO)

deriving instance MonadIO' (Ext n IO)

instance MonadTrans (Ext n) where
    lift = Ext . lift

lift' :: Monad m => n a -> Ext n m a
lift' m = Ext $ do
    r <- ask
    lift $ runMorphD r m

unlift :: Monad m => ((Ext n m a -> m a) -> m b) -> Ext n m b
unlift f = Ext $ do
    r <- ask
    lift $ f $ flip runReaderT r . unExt

runExt :: MorphD n m -> Ext n m a -> m a
runExt v (Ext m) = runReaderT m v

class MonadIO m => MonadIO' m where
    unliftIO :: (Morph m IO -> m b) -> m b

instance MonadIO' IO where
    unliftIO f = f id

instance MonadIO' (ReaderT r IO) where
    unliftIO f = do
        x <- ask
        f $ \m -> runReaderT m x

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

class Monad m => NewRef m where
    newRef' :: forall a . a -> m (MorphD (StateT a m) m)

instance NewRef IO where
    newRef' x = do
        vx <- liftIO $ newMVar x
        return $ MorphD $ \m -> modifyMVar vx $ liftM swap . runStateT m
      where
        swap (a, b) = (b, a)

instance NewRef m => NewRef (Ext n m) where
    newRef' = liftM (\m -> MorphD $ \k -> unlift $ runMorphD m . flip mapStateT k) . lift . newRef'

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


