{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | Lens-based Gtk interface
module GUI.MLens.Gtk.Interface
    ( I (..)
    , ListLayout (..)
    ) where

import Control.MLens
import Control.Monad.Register

--   forall a . (I m -> C m a) -> Receiver m a

-- | Interface description parametrized by a monad
data I m
    = Label (Receiver m String)     -- ^ label
    | Button { label_  :: Receiver m String
             , sensitive_ :: Receiver m Bool
             , action_ :: Sender m ()
             }  -- ^ button
    | Checkbox (IRef m Bool)         -- ^ checkbox
    | Combobox [String] (IRef m Int) -- ^ combo box
    | Entry (IRef m String)          -- ^ entry field
    | List ListLayout [I m]         -- ^ group interfaces into row or column
    | Notebook [(String, I m)]      -- ^ tabs
    | Cell' (forall a . (I m -> C m a) -> Receiver m a)
    | Action (C m (I m))              -- ^ do an action before giving the interface

data ListLayout
    = Horizontal | Vertical

