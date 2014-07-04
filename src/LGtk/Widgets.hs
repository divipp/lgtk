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

type Receive m a = a -> RefWriterT m ()

type SendReceive m a = (RefReaderT m a, Receive m a)

type Widget m = RefCreatorT m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (RefReaderT m String)     -- ^ label
    | Button { label_  :: RefReaderT m String
             , sensitive_ :: RefReaderT m Bool
             , color_ :: Maybe (RefReaderT m (Colour Double))
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (String -> Bool) (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (RefReaderT m b) (forall x . (Widget m -> RefCreatorT m x) -> b -> RefCreatorT m (RefCreatorT m x))
    | forall a b . (Eq b, Monoid a, Semigroup a)
    => Canvas
        Int     -- width
        Int     -- height
        Double  -- scale
        ((MouseEvent a, Dia a) -> RefWriterT m ())    -- mouse event handler
        (KeyboardHandler m)              -- keyboard event handler
        (RefReaderT m b)
        (b -> Dia a)
    | Scale Double Double Double (SendReceive m Double)


type Dia a = QDiagram B R2 a

data ListLayout
    = Horizontal
    | Vertical
        deriving (Eq)

type ScrollDirection = ListLayout

type KeyboardHandler m = Maybe (ModifiedKey -> RefWriterT m Bool)

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
