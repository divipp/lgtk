{-# LANGUAGE RankNTypes #-}
module Control.MLens.ExtRef
    ( module Control.MLens.NewRef
    -- * Monads with state expansion
    , ExtRef (extRef)
    -- * Applications
    , undoTr
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Category
import Data.Lens.Common (Lens, lens)
import Prelude hiding ((.), id)

import Control.MLens.NewRef
import Control.Monad.Restricted

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
class NewRef m => ExtRef m where

    extRef :: IRef m b -> Lens a b -> a -> C m (IRef m a)

instance (ExtRef m, Monoid w) => ExtRef (WriterT w m) where

    extRef x y a = mapC lift $ extRef x y a

instance (ExtRef m) => ExtRef (StateT s m) where

    extRef r k a = mapC lift $ extRef r k a

instance (ExtRef m) => ExtRef (ReaderT s m) where

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


