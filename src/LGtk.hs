{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main LGtk interface, ideally users should import only this module.
module LGtk
    ( -- * Lenses and references
      module Control.Monad.ExtRef
    , readRef'

    -- * Binding effects to references
    , module Control.Monad.Register
    , module Control.Monad.EffRef

    -- * Gtk structures
    , module GUI.Gtk.Structures

    -- * Running GUI descriptions
    , runWidget

    -- * Derived constructs
    , Widget
    , button
    , smartButton
    , checkbox, combobox, entry
    , vcat, hcat
    , notebook
    , cell
    ) where

import Data.Maybe
import Control.Category
import Control.Monad
import Prelude hiding ((.), id)

import Control.Monad.ExtRef hiding (liftWriteRef)
import qualified Control.Monad.ExtRef as ExtRef
import Control.Monad.Register
import Control.Monad.Register.Basic
import Control.Monad.EffRef
import GUI.Gtk.Structures hiding (Send, Receive, SendReceive, Widget)
import qualified GUI.Gtk.Structures as Gtk
import qualified GUI.Gtk.Structures.IO as Gtk
import Control.Monad.ExtRef.Pure

type Widget m = Gtk.Widget (EffectM m) m

vcat :: [Widget m] -> Widget m
vcat = List Vertical

hcat :: [Widget m] -> Widget m
hcat = List Horizontal

smartButton
  :: (EffRef m, Eq a) =>
     Send m String -> (a -> ReadRef m a) -> Ref m a -> Widget m
smartButton s f k =
    Button s (rEffect $ readRef k >>= \x -> liftM (/= x) $ f x)
             (toReceive $ \() -> runR (readRef k) >>= runR . f >>= writeRef k)

cell :: (EffRef m, Eq a) => Bool -> ReadRef m a -> (a -> m (Widget m)) -> Widget m
cell b r g = Cell (toSend b r) $ Action . g

button
    :: EffRef m
    => ReadRef m String
    -> ReadRef m (Maybe (WriteRef m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget m
button r fm = Button (rEffect r) (rEffect $ liftM isJust fm)
    (toReceive $ const $ runR fm >>= maybe (return ()) id)

checkbox :: EffRef m => Ref m Bool -> Widget m
checkbox r = Checkbox (rEffect (readRef r), register r)

combobox :: EffRef m => [String] -> Ref m Int -> Widget m
combobox ss r = Combobox ss (rEffect (readRef r), register r)

entry :: EffRef m => Ref m String -> Widget m
entry r = Entry (rEffect (readRef r), register r)

notebook :: EffRef m => [(String, Widget m)] -> Widget m
notebook xs = Action $ do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell True (liftM (== index) $ readRef currentPage) $ return . h where
           h False = hcat []
           h True = w
    return $ Notebook' (register currentPage) $ zipWith f [0..] xs

-- | Run an interface description
runWidget :: (forall m . EffIORef m => Widget m) -> IO ()
runWidget e = Gtk.gtkContext $ \post -> runExt_ $ \mo -> evalRegister (mo . ExtRef.liftWriteRef) mo $ \post' -> Gtk.runWidget liftEffectM post' post e

readRef' :: EffIORef m => Ref m a -> m a
readRef' = ExtRef.liftWriteRef . runR . readRef



