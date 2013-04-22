{-# LANGUAGE RankNTypes #-}
module Control.MLens.ExtRef
    ( module Control.MLens.NewRef
    -- * Monads with state expansion
    , ExtRef (extRef)
    -- * Applications
    , undoTr
    ) where

--import Data.IORef
import Control.Monad
import Control.Category
import Data.Lens.Common (Lens, lens)
import Prelude hiding ((.), id)

import Control.MLens.NewRef
import Data.MLens.Ref
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
    extRef :: Ref m b -> Lens a b -> a -> C m (Ref m a)


-- | Undo-redo state transformation
undoTr
    :: ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a              -- ^ reference of state
    -> C m ( R m (Maybe (m ()))   
         , R m (Maybe (m ()))
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


