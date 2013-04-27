{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.MLens.NewRef
    ( -- * Monads with reference creation
      Reference (..)
    , NewRef (..), Inner
    , IRef, modRef
    , IC (..)

    -- * Auxiliary functions
    , memoRead, memoWrite
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Data.MLens.Ref hiding (Ref(..))
import Control.Monad.Restricted

{- |
Laws for @NewRef@:

 *  Any reference created by @newRef@ should satisfy the reference laws.
-}
class (Monad m, Reference (Ref m)) => NewRef m where

    type Ref m :: * -> *

    liftInner :: Morph (Inner m) m

    newRef :: a -> C m (IRef m a)

type Inner m = RefMonad (Ref m)

type IRef m = Ref m

instance (NewRef m, Monoid w) => NewRef (WriterT w m) where

    type Ref (WriterT w m) = Ref m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

instance (NewRef m) => NewRef (StateT s m) where

    type Ref (StateT s m) = Ref m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

instance (NewRef m) => NewRef (ReaderT s m) where

    type Ref (ReaderT s m) = Ref m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

memoRead :: NewRef m => C m a -> C m (C m a)
memoRead g = liftM ($ ()) $ memoWrite $ const g

memoWrite :: (NewRef m, Eq b) => (b -> C m a) -> C m (b -> C m a)
memoWrite g = do
    s <- newRef Nothing
    return $ \b -> mapC liftInner (rToC (readRef s)) >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            unsafeC $ liftInner $ writeRef s $ Just (b, a)
            return a

data IC m a = forall b . Eq b => IC (R (Inner m) b) (b -> C m a)



