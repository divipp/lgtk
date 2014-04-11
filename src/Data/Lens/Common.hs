{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Lens.Common
  ( Lens
  , Lens'
  -- * Lens construction
  , lens -- build a lens from a getter and setter
  -- * Functional API
  , getL  -- (^.)
  , set  -- set
  , over  -- over
  -- * Stock lenses
  , _1
  , _2
  , showLens
  , listLens
  , maybeLens
  ) where

import Data.Maybe
import Control.Lens

-- | Gets the getter function from a lens.
getL :: Lens' a b -> a -> b
getL l a = a ^. l

showLens :: (Show a, Read a) => Lens' a String
showLens = lens show $ \def s -> maybe def fst $ listToMaybe $ reads s

listLens :: Lens' (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set (_, x) [] = (False, x)
    set _ (l: r) = (True, (l, r))

maybeLens :: Lens' (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\(_,a) x -> maybe (False, a) (\a' -> (True, a')) x)

