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
import Prelude hiding ((.), id)

import Control.MLens.NewRef
import Data.MLens
import Data.MLens.Ref

{- |
Suppose that @k@ is a pure lens, and

@s <- extRef r k a0@.

The following laws should hold:

 *  @s@ is a pure reference.

 *  @(k . s)@ behaves exactly as @r@.

 *  The initial value of @s@ is the result of @(readRef r >>= setL k a0)@.

Moreover, @(extRef r k a0)@ should not change the value of @r@.

The following two operations should be identical:

@newRew x@

@extRef unitLens unitLens x@

For examples, see "Control.MLens.ExtRef.Pure.Test".
-}
class NewRef m => ExtRef m where
    extRef :: Ref m b -> MLens m a b -> a -> m (Ref m a)


-- | Undo-redo state transformation
undoTr
    :: ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a              -- ^ reference of state
    -> m ( m (Maybe (m ()))   
         , m (Maybe (m ()))
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


