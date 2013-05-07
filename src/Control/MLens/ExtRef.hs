{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.MLens.ExtRef
    ( -- * Monads with reference creation
      Reference (..)
    , ExtRef (..)
    , Inner
    , IRef, modRef

    -- * Applications
    , undoTr
    , memoRead, memoWrite
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Category
import Data.Lens.Common (Lens, lens)
import Prelude hiding ((.), id)

import Control.Monad.Restricted

{- |
Laws for pure references:

 *  @(readRef r >> return ())@ === @(return ())@

 *  @(readRef r >>= writeRef r)@ === @(return ())@

 *  @(writeRef r a >> readRef r)@ === @(return a)@

 *  @(writeRef r a >> writeRef r a')@ === @(writeRef r a')@

These laws are equivalent to the get-no-effect, set-get, get-set and set-set laws for monadic lenses.
-}
class Monad (RefMonad r) => Reference r where

    type RefMonad r :: * -> *

    readRef  :: r a -> R (RefMonad r) a
    writeRef :: r a -> a -> RefMonad r ()

    (%) :: Lens a b -> r a -> r b
    joinRef :: R (RefMonad r) (r a) -> r a
    unitRef :: r ()

infixr 8 %

modRef :: Reference r => r a -> (a -> a) -> RefMonad r ()
r `modRef` f = runR (readRef r) >>= writeRef r . f

type Inner m = RefMonad (Ref m)

type IRef m = Ref m

-- | @memoRead g = liftM ($ ()) $ memoWrite $ const g@
memoRead :: ExtRef m => C m a -> C m (C m a)
memoRead g = do
    s <- newRef Nothing
    return $ mapC liftInner (rToC (readRef s)) >>= \x -> case x of
        Just a -> return a
        _ -> g >>= \a -> do
            unsafeC $ liftInner $ writeRef s $ Just a
            return a

memoWrite :: (ExtRef m, Eq b) => (b -> C m a) -> C m (b -> C m a)
memoWrite g = do
    s <- newRef Nothing
    return $ \b -> mapC liftInner (rToC (readRef s)) >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            unsafeC $ liftInner $ writeRef s $ Just (b, a)
            return a


{- |
Suppose that @r@ is a pure reference and @k@ is a pure lens.

The following laws should hold:

 *  @(extRef r k a0 >>= readRef)@ === @(readRef r >>= setL k a0)@

 *  @(extRef r k a0 >> readRef r)@ === @(readRef r)@

Given @s <- extRef r k a0@, the following laws should hold:

 *  @s@ is a pure reference

 *  @(k . s)@ === @r@

Law for @newRef@ when @extRef@ is defined:

 *  @(newRew x)@ === @(extRef unitLens unitLens x)@

For basic usage examples, look into the source of "Control.MLens.ExtRef.Pure.Test".
-}
class (Monad m, Reference (Ref m)) => ExtRef m where

    type Ref m :: * -> *

    liftInner :: Morph (Inner m) m

    newRef :: a -> C m (IRef m a)
    newRef = extRef unitRef $ lens (const ()) (const id)

    extRef :: IRef m b -> Lens a b -> a -> C m (IRef m a)

instance (ExtRef m, Monoid w) => ExtRef (WriterT w m) where

    type Ref (WriterT w m) = Ref m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

    extRef x y a = mapC lift $ extRef x y a

instance (ExtRef m) => ExtRef (StateT s m) where

    type Ref (StateT s m) = Ref m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

    extRef r k a = mapC lift $ extRef r k a

instance (ExtRef m) => ExtRef (ReaderT s m) where

    type Ref (ReaderT s m) = Ref m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

    extRef r k a = mapC lift $ extRef r k a


-- | Undo-redo state transformation
undoTr
    :: ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> IRef m a             -- ^ reference of state
    -> C m ( R (Inner m) (Maybe (Inner m ()))   
           , R (Inner m) (Maybe (Inner m ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extRef r undoLens ([], [])
    let try f = liftM (fmap (writeRef ku) . f) $ readRef ku
    return (try undo, try redo)
  where
    undoLens = lens get set where
        get = head . fst
        set x (x' : xs, ys) | eq x x' = (x: xs, ys)
        set x (xs, _) = (x : xs, [])

    undo (x: xs@(_:_), ys) = Just (xs, x: ys)
    undo _ = Nothing

    redo (xs, y: ys) = Just (y: xs, ys)
    redo _ = Nothing


