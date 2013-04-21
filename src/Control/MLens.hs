{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( -- * Data types
      MLens
    , Ref

    -- * Lens and Ref transformations
    , mapMLens
    , mapRef
    , (.)
    , (%)
    , (***)
    , joinLens
    , joinRef
    , memoMLens
    , memoRef

    -- * Lens and Ref destruction
    , runMLens
    , runRef

    -- * Lens and Ref construction
    , unitRef
    , lensStore
    , NewRef (..)
    , ExtRef (..)
    , Pure.Ext, Pure.runExt, Pure.runExt_

    -- * Derived constructs
    -- ** Lens operations
    , getL, setL, modL
    , readRef, writeRef, modRef

    -- ** Pure lenses, defined with @lensStore@
    , id
    , unitLens
    , fstLens, sndLens
    , maybeLens
    , listLens
    , ithLens

    -- ** Impure lenses, defined with @lensStore@
    , forkLens
    , justLens
    , showLens

    -- ** Other derived construts
    , Lens
    , fromLens
    , toLens
    , lens
    , undoTr
    , memoRead
    , memoWrite

    -- * Auxiliary definitions
    , Morph

    -- ** Consistency tests
    , testExtPure
    , testExtIORef
    ) where

import Control.Category
import Control.Monad.Writer
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.ExtRef.Test
import qualified Control.MLens.ExtRef.Pure as Pure
import qualified Control.MLens.ExtRef.IORef as IORef

newtype ExtTestPure i a = ExtTestPure { runExtTestPure :: Pure.Ext i (Writer [String]) a }
    deriving (Monad, MonadWriter [String], NewRef, ExtRef)

-- | Consistency tests for the pure implementation of @Ext@, should give an empty list of errors.
testExtPure :: [String]
testExtPure = mkTests $ \t -> execWriter $ Pure.runExt $ runExtTestPure t

newtype ExtTestIORef i a = ExtTestIORef { runExtTestIORef :: IORef.Ext i (Writer [String]) a }
    deriving (Monad, MonadWriter [String], NewRef, ExtRef)

-- | Consistency tests for the @IORef@-based implementation of @Ext@, should give an empty list of errors.
testExtIORef :: [String]
testExtIORef = mkTests $ \t -> execWriter $ IORef.runExt $ runExtTestIORef t


