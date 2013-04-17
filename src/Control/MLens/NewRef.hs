{-# LANGUAGE RankNTypes #-}
module Control.MLens.NewRef
    ( -- * Monads with reference creation
      NewRef (newRef)

    -- * Memo operators
    , memoMLens

    -- * Auxiliary functions
    , memoRead, memoWrite
    ) where

import Control.Monad
import Control.Monad.Writer

import Data.MLens
import Data.MLens.Ref

{- |
Laws for @NewRef@:

 *  Any reference created by @newRef@ should satisfy the reference laws given in "Data.MLens.Ref".
-}
class (Monad m) => NewRef m where
    newRef :: a -> m (Ref m a)

instance (NewRef m, Monoid w) => NewRef (WriterT w m) where
    newRef = liftM (mapMLens lift) . lift . newRef

-- | Memoise pure lenses
memoMLens :: (NewRef m, Eq a, Eq b) => MLens m a b -> m (MLens m a b)
memoMLens (MLens k) = do
    s <- newRef Nothing
    return $ MLens $ \a -> readRef s >>= \x -> do
        (b, bf) <- case x of
            Just (a', b, bf) | a' == a -> return (b, bf)
            _ -> k a >>= \(b, bf) -> do
                writeRef s $ Just (a, b, bf)
                return (b, bf)
        return (b
            , \b -> readRef s >>= \x -> case x of
                Just (a', b', _) | (a', b') == (a, b) -> return a
                Just (_, _, bf) -> bf b >>= \a -> do
                    writeRef s $ Just (a, b, bf)
                    return a
                _ -> bf b >>= \a -> do
                    writeRef s $ Just (a, b, bf)
                    return a
            )


memoRead :: NewRef m => m a -> m (m a)
memoRead g = liftM ($ ()) $ memoWrite $ const g

memoWrite :: (NewRef m, Eq b) => (b -> m a) -> m (b -> m a)
memoWrite g = do
    s <- newRef Nothing
    return $ \b -> readRef s >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            writeRef s $ Just (b, a)
            return a



