{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lens-based interface of LGtk
module LGtk.Lens
    ( module Export
    , lensMap
    , extRef
    , maybeLens
    ) where

import LGtk as Export hiding ((.), id, lensMap, extRef, lens, Lens, iso, maybeLens)
import qualified LGtk as LGtk

import Control.Applicative
import Control.Monad.Identity

---------

toLens :: Lens' s a -> LGtk.Lens s a
toLens l = LGtk.lens (getConst . l Const) (\s -> runIdentity . l (\_ -> Identity s))

lensMap = LGtk.lensMap . toLens

extRef a = LGtk.extRef a . toLens

maybeLens :: Lens' (Bool, a) (Maybe a)
maybeLens = lens g s where
    g (True, a) = Just a
    g _ = Nothing
    s _ (Just a) = (True, a)
    s (_, a) _ = (False, a)

--------- re-define to avoid dependency on lens

type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

