{-# LANGUAGE RankNTypes #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( module Data.Lens.Common

    -- * Restricted monads
    , R, runR, mapR
    , C, runC, mapC
    , rToC

    -- * Reference classes
    , Reference (..)
    , LensReference (..)

    -- * Ref construction classes
    , NewRef (..)
    , ExtRef (..)

    -- * Derived constructs
    , Inner
    , IRef
    , IC (..)
    , modRef
    , undoTr
    , memoRead

    -- * Auxiliary definitions
    , Morph

    -- * Auxiliary lens definitions
    , listLens
    , maybeLens
    , showLens

    -- * Re-exported
    , (.)
    , id
    ) where

import Control.Category
import Prelude hiding ((.), id)
import Data.Lens.Common
import Data.Maybe

import Control.Monad.Restricted
import Control.MLens.ExtRef

showLens :: (Show a, Read a) => Lens a String
showLens = lens show $ \s def -> maybe def fst $ listToMaybe $ reads s

listLens :: Lens (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set [] (_, x) = (False, x)
    set (l: r) _ = (True, (l, r))


maybeLens :: Lens (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\x (_,a) -> maybe (False, a) (\a' -> (True, a')) x)


