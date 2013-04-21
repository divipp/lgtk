{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( module Data.Lens.Common

    -- * Data types
    , Ref
    , R, runR

    -- * Ref transformations
    , M.mapRef
    , (%)
    , M.joinRef
    , M.memoRef

    -- * Ref destruction
    , M.runRef

    -- * Ref construction
    , M.unitRef
    , M.NewRef
    , M.newRef
    , M.ExtRef
    , extRef
    , Pure.Ext
    , Pure.runExt
    , Pure.runExt_

    -- * Derived constructs
    , M.readRef
    , M.writeRef
    , M.modRef
    , M.undoTr
    , M.memoRead
    , M.memoWrite

    -- * Auxiliary definitions
    , M.Morph

    -- * Auxiliary lens definitions
    , (.)
    , id
    , listLens
    , showLens

    -- * Consistency tests
    , testExtPure
    , testExtIORef
    ) where

import Control.Category
import Control.Monad.Writer
import Prelude hiding ((.), id)
import qualified Data.Lens.Common as L
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Data.Maybe

import qualified Data.MLens as M
import Data.MLens.Ref (Ref(Ref))
import qualified Data.MLens.Ref as M
import qualified Control.MLens.ExtRef as M
import Control.MLens.ExtRef.Test
import qualified Control.MLens.ExtRef.Pure as Pure
import qualified Control.MLens.ExtRef.IORef as IORef

newtype R m a = R { runR :: m a }

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

extRef :: M.ExtRef m => Ref m b -> L.Lens a b -> a -> m (Ref m a)
extRef r k a = M.extRef r (toMLens k) a

newtype ExtTestPure i a = ExtTestPure { runExtTestPure :: Pure.Ext i (Writer [String]) a }
    deriving (Monad, MonadWriter [String], M.NewRef, M.ExtRef)

-- | Consistency tests for the pure implementation of @Ext@, should give an empty list of errors.
testExtPure :: [String]
testExtPure = mkTests $ \t -> execWriter $ Pure.runExt $ runExtTestPure t

newtype ExtTestIORef i a = ExtTestIORef { runExtTestIORef :: IORef.Ext i (Writer [String]) a }
    deriving (Monad, MonadWriter [String], M.NewRef, M.ExtRef)

-- | Consistency tests for the @IORef@-based implementation of @Ext@, should give an empty list of errors.
testExtIORef :: [String]
testExtIORef = mkTests $ \t -> execWriter $ IORef.runExt $ runExtTestIORef t


