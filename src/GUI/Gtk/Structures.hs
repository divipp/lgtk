{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | Lens-based Gtk interface
module GUI.Gtk.Structures
    ( module GUI.Gtk.Structures
    , module Graphics.UI.Gtk
    , Color (..)
    ) where

import Graphics.UI.Gtk.Gdk.GC (Color (Color))
import Diagrams.Prelude (QDiagram, R2, Monoid)
import Diagrams.Backend.Cairo (Cairo)
import Graphics.UI.Gtk (ScrollDirection (..), KeyVal, Modifier, keyName, keyToChar)

import Control.Monad.EffRef (Command (..))

type Dia a = QDiagram Cairo R2 a

type Send n m a = (a -> n ()) -> m ()
type Receive n m k a = (Command -> n ()) -> m (a -> k ())
type SendReceive n m k a = (Send n m a, Receive n m k a)

type Widget n m k = m (WidgetCore n m k)

-- | Widget descriptions
data WidgetCore n m k
    = Label (Send n m String)     -- ^ label
    | Button { label_  :: Send n m String
             , sensitive_ :: Send n m Bool
             , color_ :: Send n m Color
             , action_ :: Receive n m k ()
             }  -- ^ button
    | Checkbox (SendReceive n m k Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive n m k Int) -- ^ combo box
    | Entry (SendReceive n m k String)          -- ^ entry field
    | List ListLayout [Widget n m k]         -- ^ group interfaces into row or column
    | Notebook' (Receive n m k Int) [(String, Widget n m k)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell ((b -> m (m ())) -> m ()) (forall a . (Widget n m k -> m a) -> b -> m (m a))
    | forall a b . (Eq b, Monoid a) => Canvas Int Int Double (Receive n m k (MouseEvent a)) (Send n m b) (b -> Dia a)
    | Scale Double Double Double (SendReceive n m k Double)

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
    | KeyPress [Modifier] KeyVal
        deriving (Eq)

data MousePos a
    = MousePos (Double, Double) a
        deriving (Eq)
