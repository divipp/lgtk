{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.MLens.NewRef
    ( -- * Monads with reference creation
      NewRef (..)
    , IRef, liftRef

    -- * Memo operators
    , memoRef

    -- * Auxiliary functions
    , memoRead, memoWrite
    ) where

import Control.Monad
import Control.Monad.Writer

import Data.MLens.Ref
import Control.Monad.Restricted

{- |
Laws for @NewRef@:

 *  Any reference created by @newRef@ should satisfy the reference laws.
-}
class (Monad m, Monad (Inner m)) => NewRef m where
    type Inner m :: * -> *

    liftInner :: Morph (Inner m) m

    newRef :: a -> C m (IRef m a)

type IRef m = Ref (Inner m)

liftRef :: NewRef m => IRef m a -> Ref m a
liftRef = mapRef liftInner

instance (NewRef m, Monoid w) => NewRef (WriterT w m) where
    type Inner (WriterT w m) = Inner m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef


-- | Memoise pure references
memoRef :: (NewRef m, Eq a) => IRef m a -> C m (IRef m a)
memoRef r = do
    s <- newRef Nothing
    let re = readRef s >>= \x -> case x of
                Just b -> return b
                _ -> readRef r >>= \b -> do
                    unsafeR $ writeRef s $ Just b
                    return b
        w b = runR (readRef s) >>= \x -> case x of
                Just b' | b' == b -> return ()
                _ -> do
                    writeRef s $ Just b
                    writeRef r b
    return $ Ref re w


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



