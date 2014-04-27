{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
-- | Lens-based Gtk interface
module GUI.Gtk.Structures
    ( module GUI.Gtk.Structures
    , Color (..)
    ) where

import Data.Semigroup
import Graphics.UI.Gtk.Gdk.GC (Color (Color))
import Diagrams.Prelude (QDiagram, R2)
import Diagrams.Backend.Cairo (Cairo)

import Control.Monad.ExtRef

data KeyModifier = ShiftModifier | ControlModifier deriving (Eq, Ord)

shiftKeyModifier = ShiftModifier
controlKeyModifier = ControlModifier

type Dia a = QDiagram Cairo R2 a

type Receive m a = a -> Control.Monad.ExtRef.Modifier m ()

type SendReceive m a = (ReadRef m a, Receive m a)

type Widget m = m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (ReadRef m String)     -- ^ label
    | Button { label_  :: ReadRef m String
             , sensitive_ :: ReadRef m Bool
             , color_ :: Maybe (ReadRef m Color)
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (ReadRef m b) (b -> m (Widget m))
    | forall a b . (Eq b, Monoid a, Semigroup a) => Canvas Int Int Double (Receive m (MouseEvent a)) (ReadRef m b) (b -> Dia a)
    | Scale Double Double Double (SendReceive m Double)


data ListLayout
    = Horizontal
    | Vertical
        deriving (Eq)

type ScrollDirection = ListLayout

data MouseEvent a
    = MoveTo (MousePos a)
    | MouseEnter (MousePos a)
    | MouseLeave (MousePos a)
    | Click (MousePos a)
    | DragTo (MousePos a)
    | Release (MousePos a)
    | ScrollTo ScrollDirection (MousePos a)
    | KeyPress [KeyModifier] String (Maybe Char)
    | LostFocus
        deriving (Eq)

data MousePos a
    = MousePos (Double, Double) a
        deriving (Eq)
