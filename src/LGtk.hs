{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main LGtk interface, ideally users should import only this module.
module LGtk
    ( -- * Lenses and references
      module Control.Monad.ExtRef

    -- * Binding effects to references
    , module Control.Monad.Register
    , module Control.Monad.EffRef

    -- * Gtk structures
    , module GUI.Gtk.Structures

    -- * Running GUI descriptions
    , runI

    -- * Derived constructs
    , I
    , vcat, hcat
    , cell
    , button
    , checkbox, combobox, entry
    , smartButton
    , notebook
    ) where

import Data.Maybe
import Control.Category
import Control.Monad
import Prelude hiding ((.), id)

import Control.Monad.ExtRef
import Control.Monad.Register
import Control.Monad.Register.Basic
import Control.Monad.EffRef
import GUI.Gtk.Structures
import qualified GUI.Gtk.Structures.IO as Gtk
import Control.Monad.ExtRef.Pure

type I m = Widget (EffectM m) m

vcat :: [I m] -> I m
vcat = List Vertical

hcat :: [I m] -> I m
hcat = List Horizontal

smartButton
  :: (EffRef m, Eq a) =>
     Send m String -> (a -> R (Inner m) a) -> Ref m a -> I m
smartButton s f k =
    Button s (rEffect $ readRef k >>= \x -> liftM (/= x) $ f x)
             (toReceive $ \() -> runR (readRef k) >>= runR . f >>= writeRef k)

cell :: (MonadRegister m, Eq a) => Bool -> R (PureM m) a -> (a -> C m (I m)) -> I m
cell b r g = Cell' $ \f -> toSend b r $ \x -> f $ Action $ g x 

button
    :: MonadRegister m
    => Send m String
    -> R (PureM m) (Maybe (PureM m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> I m
button r fm = Button r (rEffect $ liftM isJust fm)
    (toReceive $ const $ runR fm >>= maybe (return ()) id)

checkbox :: EffRef m => Ref m Bool -> I m
checkbox r = Checkbox (rEffect (readRef r), toReceive (writeRef r))

combobox :: EffRef m => [String] -> Ref m Int -> I m
combobox ss r = Combobox ss (rEffect (readRef r), toReceive (writeRef r))

entry :: EffRef m => Ref m String -> I m
entry r = Entry (rEffect (readRef r), toReceive (writeRef r))

notebook :: EffRef m => [(String, I m)] -> I m
notebook xs = Action $ do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ Cell' $ \mkWidget -> let
           h False = hcat []
           h True = w
         in toSend True (liftM (== index) $ readRef currentPage) $ mkWidget . h

    return $ Notebook' (toReceive $ writeRef currentPage) $ zipWith f [0..] xs

-- | Run an interface description
runI :: (forall m . EffIORef m => I m) -> IO ()
runI e = Gtk.gtkContext $ \post -> runExt_ $ \mo -> evalRegister (mo . liftInner) mo $ \post' -> Gtk.runWidget liftEffectM post' post id e

