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
  :: (MonadRegister m, Eq a, ExtRef m, Functor (Inner m), Inner m ~ Inner' m) =>
     Receiver m String -> (a -> R (Inner m) a) -> IRef m a -> I m
smartButton s f k =
    button s $ toFree $ readRef k >>= \x -> f x >>= \y -> 
        if y == x then return Nothing else return $ Just $ runR (readRef k) >>= runR . f >>= writeRef k

cell :: MonadRegister m => Bool -> IC m (I m) -> I m
cell b (IC r g) = Cell' $ \f -> addICEffect b $ IC r $ \x -> f $ Action $ g x 

button :: (MonadRegister m, Inner m ~ Inner' m, Functor (Inner m))
    => Receiver m String
    -> Free (R (Inner m)) (Maybe (Inner m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> I m
button r fm = Button r (addFreeCEffect (fmap isJust fm))
    (\f -> addPushEffect (unFree (maybe (return ()) id) (join . fmap (maybe (return ()) id) . runR) fm) $ \g -> f $ \() -> g)

-- | Run an interface description
runI :: (forall m . (Functor (Inner m), MonadRegister m, ExtRef m, Inner m ~ Inner' m, MonadIO (Inn m)) => I m) -> IO ()
runI e = Gtk.gtkContext $ \post -> runExt_ $ \mo -> evalEE (mo . liftInner) mo $ Gtk.runI post id e

toFree :: (Functor m, Monad m) => m a -> Free m a
toFree = Impure . fmap Pure


