{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.Monad.ExtRef
    ( module Data.Lens.Common

    -- * Restricted monads
    , HasReadPart (..)

    -- * Reference classes
    , Reference (..)

    -- * Ref construction classes
    , ExtRef (..)

    -- * Derived constructs
    , ReadR
    , ReadRef
    , WriteRef
    , readRef'
    , modRef
    , undoTr
    , memoRead
    , memoWrite

    -- * Auxiliary definitions
    , Morph
    , MorphD (..)
    , MonadIO' (..)

    -- * Auxiliary lens definitions
    , listLens
    , maybeLens
    , showLens

    -- * Re-exported
    , (.)
    , id
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Category
import Data.Maybe
import Data.Lens.Common
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
class (HasReadPart (RefMonad r)) => Reference r where

    type RefMonad r :: * -> *

    readRef  :: r a -> ReadR r a
    writeRef :: r a -> a -> RefMonad r ()

    (%) :: Lens a b -> r a -> r b
    joinRef :: ReadR r (r a) -> r a
    unitRef :: r ()

type ReadR r = ReadPart (RefMonad r)

infixr 8 %

modRef :: Reference r => r a -> (a -> a) -> RefMonad r ()
r `modRef` f = runR (readRef r) >>= writeRef r . f

type WriteRef m = RefMonad (Ref m)

type ReadRef m = ReadR (Ref m)

readRef' :: ExtRef m => Ref m a -> m a
readRef' = liftWriteRef . runR . readRef

-- | @memoRead g = liftM ($ ()) $ memoWrite $ const g@
memoRead :: ExtRef m => m a -> m (m a)
memoRead g = do
    s <- newRef Nothing
    return $ liftWriteRef (runR (readRef s)) >>= \x -> case x of
        Just a -> return a
        _ -> g >>= \a -> do
            liftWriteRef $ writeRef s $ Just a
            return a

memoWrite :: (ExtRef m, Eq b) => (b -> m a) -> m (b -> m a)
memoWrite g = do
    s <- newRef Nothing
    return $ \b -> liftWriteRef (runR (readRef s)) >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            liftWriteRef $ writeRef s $ Just (b, a)
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

For basic usage examples, look into the source of "Control.Monad.ExtRef.Pure.Test".
-}
class (Monad m, Reference (Ref m)) => ExtRef m where

    type Ref m :: * -> *

    liftWriteRef :: Morph (WriteRef m) m

    extRef :: Ref m b -> Lens a b -> a -> m (Ref m a)

    -- | @newRef = extRef unitRef $ lens (const ()) (const id)@
    newRef :: a -> m (Ref m a)
    newRef = extRef unitRef $ lens (const ()) (const id)

instance (ExtRef m, Monoid w) => ExtRef (WriterT w m) where

    type Ref (WriterT w m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef x y a = lift $ extRef x y a

instance (ExtRef m) => ExtRef (ReaderT s m) where

    type Ref (ReaderT s m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef r k a = lift $ extRef r k a

instance (ExtRef m, Monoid w) => ExtRef (RWST r w s m) where

    type Ref (RWST r w s m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef r k a = lift $ extRef r k a


-- | Undo-redo state transformation
undoTr
    :: ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a             -- ^ reference of state
    ->   m ( ReadRef m (Maybe (WriteRef m ()))   
           , ReadRef m (Maybe (WriteRef m ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extRef r undoLens ([], [])
    let try f = liftM (liftM (writeRef ku) . f) $ readRef ku
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

showLens :: (Show a, Read a) => Lens a String
showLens = lens show $ \s def -> maybe def fst $ listToMaybe $ reads s

listLens :: Lens (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set [] (_, x) = (False, x)
    set (l: r) _ = (True, (l, r))


maybeLens :: Lens (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\x (_,a) -> maybe (False, a) (\a' -> (True, a')) x)


