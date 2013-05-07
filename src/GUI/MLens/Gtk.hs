{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main LGtk interface, ideally users should import only this module.
module GUI.MLens.Gtk
    ( -- * Lenses and references
      module Control.MLens

    -- * GUI combinators
    , Widget (..)
    , I
    , ListLayout (..)
    , MonadRegister
    , EffRef
    , EffIORef
    , Inner'
    , IC (..)
    , constEffect
    , voidReceiver
    , rEffect
    , addICEffect
    , addWEffect

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
    ) where

import Data.Maybe
import Control.Category
import Control.Monad
import Control.Monad.Trans
import Prelude hiding ((.), id)

import Control.MLens
import Control.Monad.Register
import GUI.MLens.Gtk.Interface
import qualified GUI.MLens.Gtk.IO as Gtk
import Control.MLens.ExtRef.Pure

type I m = Widget (Inn m) m

vcat :: [I m] -> I m
vcat = List Vertical

hcat :: [I m] -> I m
hcat = List Horizontal

smartButton
  :: (EffRef m, Eq a) =>
     Receiver m String -> (a -> R (Inner m) a) -> Ref m a -> I m
smartButton s f k =
    Button s (addCEffect $ readRef k >>= \x -> liftM (== x) $ f x)
             (addWEffect $ \() -> runR (readRef k) >>= runR . f >>= writeRef k)

cell :: MonadRegister m => Bool -> IC m (I m) -> I m
cell b (IC r g) = Cell' $ \f -> addICEffect b $ IC r $ \x -> f $ Action $ g x 

button
    :: MonadRegister m
    => Receiver m String
    -> R (Inner' m) (Maybe (Inner' m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> I m
button r fm = Button r (addCEffect $ liftM isJust fm)
    (addWEffect $ const $ runR fm >>= maybe (return ()) id)

checkbox :: EffRef m => IRef m Bool -> I m
checkbox r = Checkbox (addCEffect (readRef r), addWEffect (writeRef r))

combobox :: EffRef m => [String] -> IRef m Int -> I m
combobox ss r = Combobox ss (addCEffect (readRef r), addWEffect (writeRef r))

entry :: EffRef m => IRef m String -> I m
entry r = Entry (addCEffect (readRef r), addWEffect (writeRef r))

-- | Run an interface description
runI :: (forall m . EffIORef m => I m) -> IO ()
runI e = Gtk.gtkContext $ \post -> runExt_ $ \mo -> evalEE (mo . liftInner) mo $ Gtk.runWidget post id e

