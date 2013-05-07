{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main LGtk interface, ideally users should import only this module.
module GUI.MLens.Gtk
    ( -- * Lenses and references
      module Control.MLens

    -- * GUI combinators
    , I (..)
    , ListLayout (..)
    , MonadRegister
    , Inner'
    , IC (..)
    , constEffect
    , rEffect
    , addICEffect

    -- * File system
    , fileRef

    -- * Running GUI descriptions
    , runI

    -- * Derived constructs
    , vcat, hcat
    , cell
    , button
    , checkbox, combobox, entry
    , smartButton

    -- * Auxiliary functions
    , toFree
    ) where

import Data.Maybe
import Control.Category
import Control.Monad.Trans
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
  :: (MonadRegister m, Eq a, ExtRef m, Inner m ~ Inner' m) =>
     Receiver m String -> (a -> R (Inner m) a) -> IRef m a -> I m
smartButton s f k =
    Button s (addCEffect $ readRef k >>= \x -> liftM (== x) $ f x)
             (addWEffect $ \() -> runR (readRef k) >>= runR . f >>= writeRef k)

cell :: MonadRegister m => Bool -> IC m (I m) -> I m
cell b (IC r g) = Cell' $ \f -> addICEffect b $ IC r $ \x -> f $ Action $ g x 

button :: (MonadRegister m, Functor (Inner' m))
    => Receiver m String
    -> Free (R (Inner' m)) (Maybe (Inner' m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> I m
button r fm = Button r (addFreeCEffect (fmap isJust fm))
    (addWEffect $ \() -> unFree (maybe (return ()) id) (join . fmap (maybe (return ()) id) . runR) fm)

checkbox :: (MonadRegister m, ExtRef m, Inner m ~ Inner' m) => IRef m Bool -> I m
checkbox r = Checkbox (addCEffect (readRef r), addWEffect (writeRef r))

combobox :: (MonadRegister m, ExtRef m, Inner m ~ Inner' m) => [String] -> IRef m Int -> I m
combobox ss r = Combobox ss (addCEffect (readRef r), addWEffect (writeRef r))

entry :: (MonadRegister m, ExtRef m, Inner m ~ Inner' m) => IRef m String -> I m
entry r = Entry (addCEffect (readRef r), addWEffect (writeRef r))

-- | Run an interface description
runI :: (forall m . (Functor (Inner m), MonadRegister m, ExtRef m, Inner m ~ Inner' m, MonadIO (Inn m)) => I m) -> IO ()
runI e = Gtk.gtkContext $ \post -> runExt_ $ \mo -> evalEE (mo . liftInner) mo $ Gtk.runI post id e

toFree :: (Functor m, Monad m) => m a -> Free m a
toFree = Impure . fmap Pure


