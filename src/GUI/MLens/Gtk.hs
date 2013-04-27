{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main LGtk interface, ideally users should import only this module.
module GUI.MLens.Gtk
    ( -- * Lenses and references
      module Control.MLens

    -- * GUI combinators
    , I (..)
    , ListLayout (..)
    , MonadRegister
    , constEffect
    , rEffect
    , addICEffect

    -- * File system
    , FileSystem (..)

    -- * Running GUI descriptions
    , runI

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
import Control.MLens.ExtRef.Pure

vcat :: [I m] -> I m
vcat = List Vertical

hcat :: [I m] -> I m
hcat = List Horizontal

smartButton
  :: (Eq a, NewRef m, Functor (Inner m)) =>
     Receiver m String -> (a -> R (Inner m) a) -> IRef m a -> I m
smartButton s f k =
    Button s $ toFree $ readRef k >>= \x -> f x >>= \y -> 
        if y == x then return Nothing else return $ Just $ runR (readRef k) >>= runR . f >>= writeRef k

-- | Run an interface description
runI :: (forall m . (Functor (Inner m), MonadRegister m, ExtRef m, FileSystem m) => I m) -> IO ()
runI e = runExt_ $ \mo -> evalEE $ \_ -> Gtk.runI mo e

toFree :: (Functor m, Monad m) => m a -> Free m a
toFree = Impure . fmap Pure


