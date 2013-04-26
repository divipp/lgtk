{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main LGtk interface, ideally users should import only this module.
module GUI.MLens.Gtk
    ( -- * Lenses and references
      module Control.MLens

    -- * GUI combinators
    , I (..)
    , ListLayout (..)

    -- * Running GUI descriptions
    , runI
    , unsafeRunI

    -- * Derived constructs
    , vcat, hcat
    , smartButton

    -- * Auxiliary functions
    , toFree
    ) where

import Control.Category
import Control.Monad.Free
import Prelude hiding ((.), id)

import Control.MLens
import Control.Monad.Register
import GUI.MLens.Gtk.Interface
import qualified GUI.MLens.Gtk.IO as Gtk

vcat :: [I m] -> I m
vcat = List Vertical

hcat :: [I m] -> I m
hcat = List Horizontal

smartButton
  :: (Eq a, Monad (Inner m), Functor (Inner m)) =>
     Free (C (Inner m)) String -> (a -> C (Inner m) a) -> IRef m a -> I m
smartButton s f k =
    Button s $ toFree $ rToC (readRef k) >>= \x -> f x >>= \y -> 
        if y == x then return Nothing else return $ Just $ runR (readRef k) >>= runC . f >>= writeRef k

-- | Run an interface description
runI :: (forall m . (Functor m, ExtRef m) => I m) -> IO ()
runI e = unsafeRunI e

-- | Run an interface description
--
-- Unsafe only if you do nasty things in the @IO@ monad, like forking threads
unsafeRunI :: (forall i . I (EE (Ext_ i IO))) -> IO ()
unsafeRunI e = runExt_ $ \mo -> evalEE $ \_ -> Gtk.runI mo e

toFree :: (Functor m, Monad m) => m a -> Free m a
toFree = Impure . fmap Pure


