{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( -- * Data types
      MLens
    , Ref

    -- * Lens transformations
    , mapMLens
    , (.)
    , (***)
    , joinML
    , memoMLens

    -- * Lens destruction
    , runMLens
    , runRef

    -- * Lens construction
    , lensStore
    , NewRef (..)
    , ExtRef (..)
    , Ext, runExt, runExt_

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
    , joinLens
    , undoTr
    , memoRead
    , memoWrite

    -- * Auxiliary definitions
    , Morph

    -- ** Consistency tests
    , testExt
    ) where

import Control.Category
import Control.Monad.Writer
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.ExtRef.Test
import Control.MLens.ExtRef.Pure

newtype ExtTest i a = ExtTest { unExtTest :: Ext i (Writer [String]) a }
    deriving (Monad, MonadWriter [String], NewRef, ExtRef)

-- | Consistency tests for @Ext@, should give an empty list of errors.
testExt :: [String]
testExt = mkTests (\t -> execWriter $ runExt $ unExtTest t)


