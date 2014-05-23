{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | Lens-based Gtk interface
module LGtk.Widgets
    ( module LGtk.Widgets
    , Colour, RGB (..), sRGB, toSRGB
    ) where

import Data.Semigroup
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Prelude (QDiagram, R2, P2)
import Diagrams.Backend.Cairo (B)

---------------------------------------------------------

import Data.LensRef

import LGtk.Key

---------------------------------------------------------

type Receive m a = a -> RefWriter m ()

type SendReceive m a = (RefReader m a, Receive m a)

type Widget m = m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (RefReader m String)     -- ^ label
    | Button { label_  :: RefReader m String
             , sensitive_ :: RefReader m Bool
             , color_ :: Maybe (RefReader m (Colour Double))
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (String -> Bool) (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (RefReader m b) (forall x . (Widget m -> m x) -> b -> m (m x))
    | forall a b . (Eq b, Monoid a, Semigroup a)
    => Canvas
        Int     -- width
        Int     -- height
        Double  -- scale
        ((MouseEvent a, Dia a) -> RefWriter m ())    -- mouse event handler
        (KeyboardHandler (RefWriter m))              -- keyboard event handler
        (RefReader m b)
        (b -> Dia a)
    | Scale Double Double Double (SendReceive m Double)


type Dia a = QDiagram B R2 a

data ListLayout
    = Horizontal
    | Vertical
        deriving (Eq)

type ScrollDirection = ListLayout

type KeyboardHandler m = Maybe (ModifiedKey -> m Bool)

data MouseEvent a
    = MoveTo (MousePos a)
    | MouseEnter (MousePos a)
    | MouseLeave (MousePos a)
    | Click (MousePos a)
    | DragTo (MousePos a)
    | Release (MousePos a)
    | ScrollTo ScrollDirection (MousePos a)
    | LostFocus
    | GetFocus
        deriving (Eq)

data MousePos a
    = MousePos P2 a
        deriving (Eq)
