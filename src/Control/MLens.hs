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
    , readRef, writeRef, modRef

    -- * Lens transformations
    , fromLens, toLens
    , mapMLens
    , (.)
    , (***)
    , joinML, joinLens
    , memoMLens

    -- * Lens creation
    , lens
    , NewRef (..)
    , ExtRef (..)
    , Ext, runExt, runExt_

    -- * Derived constructs
    -- ** Pure lenses, built with @lens@
    , id
    , unitLens
    , fstLens, sndLens
    , maybeLens
    , listLens
    , ithLens

    -- ** Impure lenses, built with @lens@
    , forkLens
    , justLens
    , showLens

    -- ** Other derived construts
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


