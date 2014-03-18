{-# LANGUAGE RankNTypes #-}
module Control.Monad.Restricted where

import Data.Monoid
import Control.Concurrent
import Control.Monad.State
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

-------------------

class (Monad m) => NewRef m where
    newRef' :: a -> m (MorphD (StateT a m) m)

instance NewRef IO where
    newRef' x = do
        vx <- liftIO $ newMVar x
        return $ MorphD $ \m -> modifyMVar vx $ liftM swap . runStateT m
      where
        swap (a, b) = (b, a)

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

-------------------

newtype MonadMonoid a = MonadMonoid { runMonadMonoid :: a () }

instance Monad m => Monoid (MonadMonoid m) where
    mempty = MonadMonoid $ return ()
    MonadMonoid a `mappend` MonadMonoid b = MonadMonoid $ a >> b


