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
    , (M.%)
    , M.joinRef
    , M.memoRef

    -- * Ref operations
    , M.readRef
    , M.writeRef

    -- * Ref construction
    , M.unitRef
    , M.NewRef
    , M.newRef
    , M.ExtRef
    , M.extRef
    , Pure.Ext
    , Pure.runExt
    , Pure.runExt_

    -- * Derived constructs
    , M.modRef
    , M.undoTr
    , M.memoRead
    , M.memoWrite

    -- * Auxiliary definitions
    , Morph

    -- * Auxiliary lens definitions
    , (.)
    , id
    , listLens
    , maybeLens
    , showLens
    ) where

import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Lens.Common as L
import Data.Lens.Common
import Data.Maybe

import Control.Monad.Restricted
import Data.MLens.Ref
import qualified Data.MLens.Ref as M
import qualified Control.MLens.ExtRef as M
import qualified Control.MLens.ExtRef.Pure as Pure

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


