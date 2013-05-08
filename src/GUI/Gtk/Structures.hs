{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | Lens-based Gtk interface
module GUI.Gtk.Structures
    ( Send
    , Receive
    , SendReceive
    , Widget (..)
    , ListLayout (..)
    ) where

type Send n m a = (a -> n ()) -> m ()
type Receive n m a = ((a -> n ()) -> n ()) -> m ()
type SendReceive n m a = (Send n m a, Receive n m a)

-- | Widget descriptions
data Widget n m
    = Label (Send n m String)     -- ^ label
    | Button { label_  :: Send n m String
             , sensitive_ :: Send n m Bool
             , action_ :: Receive n m ()
             }  -- ^ button
    | Checkbox (SendReceive n m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive n m Int) -- ^ combo box
    | Entry (SendReceive n m String)          -- ^ entry field
    | List ListLayout [Widget n m]         -- ^ group interfaces into row or column
    | Notebook' (Receive n m Int) [(String, Widget n m)]     -- ^ actual tab index, tabs
    | Cell' (forall a . (Widget n m -> m a) -> Send n m a)
    | Action (m (Widget n m))              -- ^ do an action before giving the interface

data ListLayout
    = Horizontal | Vertical

