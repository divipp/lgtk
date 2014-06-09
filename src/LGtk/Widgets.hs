{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
-- | Lens-based Gtk interface
module LGtk.Widgets
    ( module LGtk.Widgets
    , Colour, RGB (..), sRGB, toSRGB
    ) where

import Data.Semigroup
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Prelude (QDiagram, R2, P2)
#ifdef __RASTERIFIC__
import Diagrams.Backend.Rasterific (B)
#else
import Diagrams.Backend.Cairo (B)
#endif

---------------------------------------------------------

import Data.LensRef

import LGtk.Key

---------------------------------------------------------

type Receive m a = a -> RefWriterOf m ()

type SendReceive m a = (RefReaderOf m a, Receive m a)

type Widget m = m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (RefReaderOf m String)     -- ^ label
    | Button { label_  :: RefReaderOf m String
             , sensitive_ :: RefReaderOf m Bool
             , color_ :: Maybe (RefReaderOf m (Colour Double))
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (String -> Bool) (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (RefReaderOf m b) (forall x . (Widget m -> m x) -> b -> m (m x))
    | forall a b . (Eq b, Monoid a, Semigroup a)
    => Canvas
        Int     -- width
        Int     -- height
        Double  -- scale
        ((MouseEvent a, Dia a) -> RefWriterOf m ())    -- mouse event handler
        (KeyboardHandler (RefWriterOf m))              -- keyboard event handler
        (RefReaderOf m b)
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
