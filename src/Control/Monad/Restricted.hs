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

{- |
Monad morphism. Think as @m@ is a submonad of @n@.
-}
type Morph m n = forall a . m a -> n a

{- |
The @MorphD@ type is needed only to avoid impredicative types.
We use @MorphD@ instead of @Morph@ when the morphism is stored inside a data structure.
-}
newtype MorphD m n = MorphD { runMorphD :: Morph m n }

-- | @m@ is a monad which has a submonad @ReadPart m@ which is isomorphic to 'Reader'.
class (Monad m, Monad (ReadPart m)) => HasReadPart m where

    {- | Law: @(ReadPart m)@  ===  @('Reader' x)@ for some @x@.

    Alternative laws which ensures this isomorphism (@r :: ReadPart m a@ is arbitrary):

     *  @(r >> return ())@ === @return ()@

     *  @liftM2 (,) r r@ === @liftM (\a -> (a, a)) r@

    See also <http://stackoverflow.com/questions/16123588/what-is-this-special-functor-structure-called>
    -}
    type ReadPart m :: * -> *

    -- | @ReadPart m@ is a submonad of @m@
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

class Monad m => SafeIO m where

    getArgs     :: m [String]
    getProgName :: m String
    lookupEnv   :: String -> m (Maybe String)

instance SafeIO IO where

    getArgs     = Env.getArgs
    getProgName = Env.getProgName
    lookupEnv   = Env.lookupEnv

instance SafeIO m => SafeIO (Ext n m) where

    getArgs     = lift getArgs
    getProgName = lift getProgName
    lookupEnv   = lift . lookupEnv

instance SafeIO m => SafeIO (IdentityT m) where

    getArgs     = lift getArgs
    getProgName = lift getProgName
    lookupEnv   = lift . lookupEnv

instance (SafeIO m, Monoid w) => SafeIO (RWST r w s m) where

    getArgs     = lift getArgs
    getProgName = lift getProgName
    lookupEnv   = lift . lookupEnv

class Monad m => NewRef m where
    newRef' :: forall a . a -> m (MorphD (State a) m)

instance NewRef IO where
    newRef' x = do
        vx <- liftIO $ newMVar x
        return $ MorphD $ \m -> liftIO $ modifyMVar vx $ return . swap . runState m
      where
        swap (a, b) = (b, a)

instance NewRef m => NewRef (Ext n m) where
    newRef' a = liftM (\m -> MorphD $ lift . runMorphD m) $ lift $ newRef' a


newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


