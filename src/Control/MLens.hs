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
    , mapMLens
    , (.)
    , (***)
    , joinML
    , memoMLens

    -- * Lens creation
    , lensStore
    , NewRef (..)
    , ExtRef (..)
    , Ext, runExt, runExt_

    -- * Derived constructs
    -- ** Pure lenses, built with @lensStore@
    , id
    , unitLens
    , fstLens, sndLens
    , maybeLens
    , listLens
    , ithLens

    -- ** Impure lenses, built with @lensStore@
    , forkLens
    , justLens
    , showLens

    -- ** Other derived construts
    , lens
    , fromLens
    , toLens
    , joinLens
    , undoTr
    , memoRead
    , memoWrite

    -- * Auxiliary definitions
    , Morph
    ) where

import Control.Category
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.ExtRef.Pure


