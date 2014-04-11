{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Lens.Common
  ( Lens
  , Lens'
  -- * Lens construction
  , lens -- build a lens from a getter and setter
  -- * Functional API
  , getL  -- (^.)
  , setL  -- set
  , modL  -- over
  -- * Stock lenses
  , _1
  , _2
  , showLens
  , listLens
  , maybeLens
  ) where

import Data.Maybe
import Control.Applicative
import Control.Monad.Identity

--------- re-define to avoid dependency on lens
type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

-- | build a lens out of an isomorphism
iso :: (s -> a) -> (b -> t) -> Lens s t a b
iso f g = lens f $ flip $ const . g

-- | Gets the getter function from a lens.
getL :: Lens' a b -> a -> b
getL l = getConst . l Const

-- | Gets the setter function from a lens.
setL :: Lens s t a b -> b -> s -> t
setL l s = runIdentity . l (const $ Identity s)

-- | Gets the modifier function from a lens.
modL :: Lens s t a b -> (a -> b) -> s -> t
modL l f = runIdentity . l (Identity . f)

-- * Stock lenses
_1 :: Lens (x,b) (y,b) x y
_1 = lens fst $ \(a,b) x -> (x,b)

_2 :: Lens (a,x) (a,y) x y
_2 = lens snd $ \(a,b) x -> (a,x)

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

