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
    , MousePos (..)
    , Color (..)
    , Modifier (..)
    , KeyVal
    , keyName
    , keyToChar
    , Dia
    , Monoid
    , Semigroup
    ) where

--import Graphics.UI.Gtk (Color)
import Graphics.UI.Gtk.Gdk.GC (Color (Color))
import Diagrams.Prelude (QDiagram, R2, Monoid, Semigroup)
import Diagrams.Backend.Cairo (Cairo)
import Graphics.UI.Gtk (ScrollDirection (..), KeyVal, Modifier, keyName, keyToChar)

import Control.Monad.Register (Command (..))

type Dia a = QDiagram Cairo R2 a

type Send n m a = (a -> n ()) -> m ()
--type Receive n m a = ((a -> n ()) -> n (Command -> n ())) -> m (Command -> n ())
type Receive n m a = (Command -> n ()) -> m (a -> n ())
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
    | forall a b . (Eq b, Eq a, Monoid a) => Canvas Int Int Double (Receive n m (MouseEvent a)) (Send n m b) (b -> Dia a)
    | Scale Double Double Double (SendReceive n m Double)

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
