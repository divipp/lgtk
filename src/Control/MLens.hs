{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( module Data.Lens.Common

    -- * Data types
    , Ref
    , R, runR, mapR
    , C, runC, mapC
    , rToC

    -- * Ref transformations
    , M.mapRef
    , (%)
    , joinRef
    , memoRef

    -- * Ref destruction
    , runRef

    -- * Ref construction
    , M.unitRef
    , M.NewRef
    , newRef
    , M.ExtRef
    , extRef
    , Pure.Ext
    , Pure.runExt
    , Pure.runExt_

    -- * Derived constructs
    , readRef
    , M.writeRef
    , M.modRef
    , undoTr
    , memoRead
    , memoWrite

    -- * Auxiliary definitions
    , M.Morph

    -- * Auxiliary lens definitions
    , (.)
    , id
    , listLens
    , maybeLens
    , showLens
    ) where

import Control.Category
import Control.Monad.Writer
import Prelude hiding ((.), id)
import qualified Data.Lens.Common as L
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Data.Maybe

import Control.Monad.Restricted
import qualified Data.MLens as M
import Data.MLens.Ref (Ref(Ref))
import qualified Data.MLens.Ref as M
import qualified Control.MLens.ExtRef as M
import qualified Control.MLens.ExtRef.Pure as Pure

joinRef :: Monad m => R m (Ref m a) -> Ref m a
joinRef (R x) = M.joinRef x

runRef :: Monad m => Ref m a -> R m (a, a -> m ())
runRef = R . M.runRef

readRef :: Monad m => Ref m a -> R m a
readRef = liftM fst . runRef

(%) :: Monad m => L.Lens a b -> Ref m a -> Ref m b
l % Ref k = Ref $ toMLens l
                . k

infixr 8 %

toMLens :: Monad m => L.Lens a b -> M.MLens m a b
toMLens l = M.lensStore (\a -> let (fb, b) = runStore $ L.runLens l a in (b, fb))

showLens :: (Show a, Read a) => L.Lens a String
showLens = L.lens show $ \s def -> maybe def fst $ listToMaybe $ reads s

listLens :: L.Lens (Bool, (a, [a])) [a]
listLens = L.lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set [] (_, x) = (False, x)
    set (l: r) _ = (True, (l, r))


maybeLens :: Lens (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\x (_,a) -> maybe (False, a) (\a' -> (True, a')) x)

newRef :: M.NewRef m => a -> C m (Ref m a)
newRef = C . M.newRef

extRef :: M.ExtRef m => Ref m b -> L.Lens a b -> a -> C m (Ref m a)
extRef r k a = C $ M.extRef r (toMLens k) a

memoRef :: (M.NewRef m, Eq a) => Ref m a -> C m (Ref m a)
memoRef = C . M.memoRef

memoRead :: M.NewRef m => C m a -> C m (C m a)
memoRead g = liftM ($ ()) $ memoWrite $ const g

memoWrite :: (M.NewRef m, Eq b) => (b -> C m a) -> C m (b -> C m a)
memoWrite f = liftM (C .) $ C $ M.memoWrite $ runC . f

-- | Undo-redo state transformation
undoTr
    :: M.ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a              -- ^ reference of state
    -> C m ( R m (Maybe (m ()))   
           , R m (Maybe (m ()))
           )  -- ^ undo and redo actions
undoTr eq r = liftM (\(u,r) -> (R u, R r)) $ C $ M.undoTr eq r


