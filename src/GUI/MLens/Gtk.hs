{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module GUI.MLens.Gtk -- --> GUI.MLens.Gtk.Interface
    ( module Control.Category
    , module Data.MLens
    , module Data.MLens.Ref
    , module Control.MLens.ExtRef
    , module GUI.MLens.Gtk.Interface

    -- * Composed
    , vcat, hcat
    , smartButton

    -- * Auxiliary functions
    , mapI
    , toFree
    ) where

import Control.Category
import Control.Monad.Free
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import GUI.MLens.Gtk.Interface

vcat :: [I m] -> I m
vcat = List Vertical

hcat :: [I m] -> I m
hcat = List Horizontal

smartButton
  :: (Eq a, Monad m, Functor m) =>
     Free m String -> (a -> m a) -> MLens m () a -> I m
smartButton s f k =
    Button s $ toFree $ readRef k >>= \x -> f x >>= \y -> 
        if y == x then return Nothing else return $ Just ((readRef k >>= f) >>= writeRef k)


mapI :: (Monad m, Functor m, Monad n, Functor n) => Morph n m -> Morph m n -> I m -> I n
mapI _g f (Label s)     = Label $ mapFree f s
mapI _g f (Button s m)  = Button (mapFree f s) (mapFree f $ fmap (fmap f) m)
mapI _g f (Entry m)     = Entry $ mapMLens f m
mapI _g f (Checkbox m)  = Checkbox $ mapMLens f m
mapI _g f (Combobox ss m) = Combobox ss $ mapMLens f m
mapI g f (List o is)    = List o $ map (mapI g f) is
mapI g f (Notebook is)  = Notebook $ map (fmap $ mapI g f) is
mapI g f (Cell b m k)   = Cell b (f m) $ mapI g f . k
mapI g f (Action m)     = Action $ f $ liftM (mapI g f) m


toFree :: (Functor m, Monad m) => m a -> Free m a
toFree = Impure . fmap Pure


