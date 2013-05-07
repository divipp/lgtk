{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | Lens-based Gtk interface
module GUI.MLens.Gtk.Interface
    ( Widget (..)
    , ListLayout (..)
    ) where

import Control.Monad.Restricted

type Receiver n m a = (a -> n ()) -> m ()
type Sender n m a = ((a -> n ()) -> n ()) -> m ()
type RS n m a = (Receiver n m a, Sender n m a)

-- | Widget descriptions
data Widget n m
    = Label (Receiver n m String)     -- ^ label
    | Button { label_  :: Receiver n m String
             , sensitive_ :: Receiver n m Bool
             , action_ :: Sender n m ()
             }  -- ^ button
    | Checkbox (RS n m Bool)         -- ^ checkbox
    | Combobox [String] (RS n m Int) -- ^ combo box
    | Entry (RS n m String)          -- ^ entry field
    | List ListLayout [Widget n m]         -- ^ group interfaces into row or column
    | Notebook' (Sender n m Int) [(String, Widget n m)]     -- ^ actual tab index, tabs
    | Cell' (forall a . (Widget n m -> C m a) -> Receiver n m a)
    | Cell'' (forall a . (Widget n m -> C m a) -> Receiver n m (Maybe a))   -- ^ auxiliary, hide it
    | Action (C m (Widget n m))              -- ^ do an action before giving the interface

data ListLayout
    = Horizontal | Vertical

