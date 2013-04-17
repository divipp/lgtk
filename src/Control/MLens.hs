-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE RankNTypes #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( -- * Data types
      MLens
    , Lens
    , Ref

    -- * Lens operations
    , getL, setL, modL

    -- * Lens transformations
    , fromLens, toLens
    , mapMLens
    , (.)
    , (***)
    , joinML, joinLens
    , memoMLens

    -- * Pure lenses
    , id
    , unitLens
    , fstLens, sndLens
    , maybeLens
    , listLens
    , ithLens

    -- * Impure lenses
    , lens
    , forkLens
    , justLens
    , showLens

    -- * Reference operations
    , readRef, writeRef, modRef
    , NewRef (..)
    , ExtRef (..)
    , Ext, runExt, runExt_
    , undoTr

    -- * Auxiliary definitions
    , Morph
    , memoRead
    , memoWrite
    ) where

import Control.Category
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.ExtRef.Pure


