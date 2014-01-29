{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | Lens-based Gtk interface
module GUI.Gtk.Structures
    ( Send
    , Receive
    , SendReceive
    , Widget (..)
    , ListLayout (..)
    , MouseEvent (..)
    , ScrollDirection (..)
    , MousePos
    , Color (..)
    , Dia
    ) where

--import Graphics.UI.Gtk (Color)
import Graphics.UI.Gtk.Gdk.GC (Color (Color))
import Diagrams.Prelude (Diagram, R2)
import Diagrams.Backend.Cairo (Cairo)
import Graphics.UI.Gtk (ScrollDirection (..))

import Control.Monad.Register (Command (..))

type Dia = Diagram Cairo R2

type Send n m a = (a -> n ()) -> m ()
type Receive n m a = ((a -> n ()) -> n (Command -> n ())) -> m (Command -> n ())
type SendReceive n m a = (Send n m a, Receive n m a)

-- | Widget descriptions
data Widget n m
    = Label (Send n m String)     -- ^ label
    | Button { label_  :: Send n m String
             , sensitive_ :: Send n m Bool
             , color_ :: Send n m Color
             , action_ :: Receive n m ()
             }  -- ^ button
    | Checkbox (SendReceive n m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive n m Int) -- ^ combo box
    | Entry (SendReceive n m String)          -- ^ entry field
    | List ListLayout [Widget n m]         -- ^ group interfaces into row or column
    | Notebook' (Receive n m Int) [(String, Widget n m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell ((b -> m (m ())) -> m ()) (forall a . (Widget n m -> m a) -> b -> m (m a))
    | Action (m (Widget n m))              -- ^ do an action before giving the interface
    | forall b . Eq b => Canvas Int Int Double (Receive n m MouseEvent) (Send n m b) (b -> Dia)
    | Scale Double Double Double (SendReceive n m Double)

data ListLayout
    = Horizontal
    | Vertical
        deriving (Eq)

data MouseEvent
    = MoveTo MousePos
    | MouseEnter MousePos
    | MouseLeave MousePos
    | Click MousePos
    | DragTo MousePos
    | Release MousePos
    | ScrollTo ScrollDirection MousePos
        deriving (Eq)

type MousePos = (Double, Double)