{-# LANGUAGE ExistentialQuantification #-}
-- | Lens-based Gtk interface
module LGtk.Widgets
    ( module LGtk.Widgets
    , Colour, RGB (..), sRGB, toSRGB
    ) where

import Data.Semigroup
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Prelude (QDiagram, R2)
import Diagrams.Backend.Cairo (Cairo)

import Data.LensRef

type Receive m a = a -> Modifier m ()

type SendReceive m a = (ReadRef m a, Receive m a)

type Widget m = m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (ReadRef m String)     -- ^ label
    | Button { label_  :: ReadRef m String
             , sensitive_ :: ReadRef m Bool
             , color_ :: Maybe (ReadRef m (Colour Double))
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

data KeyModifier
    = ShiftModifier
    | ControlModifier
        deriving (Eq, Ord)

type Dia a = QDiagram Cairo R2 a

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
