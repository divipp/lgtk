{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
-- | Lens-based Gtk interface
module GUI.Gtk.Structures
    ( module GUI.Gtk.Structures
    , module Graphics.UI.Gtk
    , Color (..), Modifier (..)
    ) where

import Data.Semigroup
import Graphics.UI.Gtk.Gdk.GC (Color (Color))
import Diagrams.Prelude (QDiagram, R2, Monoid)
import Diagrams.Backend.Cairo (Cairo)
import Graphics.UI.Gtk (ScrollDirection (..), KeyVal, Modifier (Shift, Control), keyName, keyToChar)

import Control.Monad.ExtRef
import Control.Monad.EffRef

type Dia a = QDiagram Cairo R2 a

data Send m a
    = Eq a => Send (ReadRef m a)
    | SendNothing

type Receive m a = a -> WriteRef m ()

runSend :: (EffRef m) => Send m a -> (a -> EffectM m ()) -> m ()
runSend SendNothing = const $ return ()
runSend (Send r) = rEffect r

noREffect = SendNothing

runReceive :: EffRef m => Receive m a -> (Command -> EffectM m ()) -> m (a -> CallbackM m ())
runReceive = toReceive

type SendReceive m a = (Send m a, Receive m a)

type Widget m = m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (Send m String)     -- ^ label
    | Button { label_  :: Send m String
             , sensitive_ :: Send m Bool
             , color_ :: Send m Color
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (Send m b) (forall x . (Widget m -> m x) -> b -> m (m x))
    | forall a b . (Eq b, Monoid a, Semigroup a) => Canvas Int Int Double (Receive m (MouseEvent a)) (Send m b) (b -> Dia a)
    | Scale Double Double Double (SendReceive m Double)


data ListLayout
    = Horizontal
    | Vertical
        deriving (Eq)

data MouseEvent a
    = MoveTo (MousePos a)
    | MouseEnter (MousePos a)
    | MouseLeave (MousePos a)
    | Click (MousePos a)
    | DragTo (MousePos a)
    | Release (MousePos a)
    | ScrollTo ScrollDirection (MousePos a)
    | KeyPress [Modifier] KeyVal String (Maybe Char)
    | LostFocus
        deriving (Eq)

data MousePos a
    = MousePos (Double, Double) a
        deriving (Eq)
