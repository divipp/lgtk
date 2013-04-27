{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | The main monadic lens interface, ideally users should import only this module.
module Control.MLens
    ( module Data.Lens.Common

    -- * Data types
    , Reference
    , Ref
    , IC (..)
    , R, runR, mapR
    , C, runC, mapC
    , rToC

    -- * Ref transformations
    , (%)
    , joinRef

    -- * Ref operations
    , readRef
    , writeRef

    -- * Ref construction
    , unitRef
    , NewRef
    , newRef
    , Inner, liftInner, IRef
    , ExtRef
    , extRef

    -- * Derived constructs
    , modRef
    , undoTr
    , memoRead

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


