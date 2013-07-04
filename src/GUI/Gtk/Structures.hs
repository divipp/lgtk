{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | Lens-based Gtk interface
module GUI.Gtk.Structures
    ( Send
    , Receive
    , SendReceive
    , Widget (..)
    , ListLayout (..)
    , Color (..)
    ) where

--import Graphics.UI.Gtk (Color)
import Graphics.UI.Gtk.Gdk.GC (Color (Color))

import Control.Monad.Register (Command (..))

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
    | Entry (Send n m String) (Receive n m String) (Receive n m String) (Receive n m String) (Send n m Int)         -- ^ entry field: automatic update, user input on enter, user input on focus-out event, user input on typing, focus grab
    | List ListLayout [Widget n m]         -- ^ group interfaces into row or column
    | Notebook' (Receive n m Int) [(String, Widget n m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell ((b -> m (m ())) -> m ()) (forall a . (Widget n m -> m a) -> b -> m (m a))
    | Action (m (Widget n m))              -- ^ do an action before giving the interface

data ListLayout
    = Horizontal | Vertical

