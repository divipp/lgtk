{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | The main LGtk interface, ideally users should import only this module.
module GUI.MLens.Gtk
    ( module Control.Category
    , module Data.MLens
    , module Data.MLens.Ref
    , module Control.MLens.ExtRef
    , module GUI.MLens.Gtk.Interface

    -- * Running (rendering) and interface description
    , runI
    , unsafeRunI

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
import Control.MLens.ExtRef.Pure
import qualified GUI.MLens.Gtk.IO as Gtk

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

-- | Run (render) and interface description
runI :: (forall m . (Functor m, ExtRef m) => I m) -> IO ()
runI e = runExt_ mapI e >>= Gtk.runI

-- | Run (render) and interface description
--
-- Unsafe only if you do nasty things in the @IO@ monad, like forking threads
unsafeRunI :: (forall i . I (Ext i IO)) -> IO ()
unsafeRunI e = runExt_ mapI e >>= Gtk.runI


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


