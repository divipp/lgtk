{-# LANGUAGE ExistentialQuantification #-}
-- | Lens-based Gtk interface
module GUI.MLens.Gtk.Interface
    ( I (..)
    , ListLayout (..)
    ) where

import Control.Monad.Free

import Control.MLens

-- | Interface description parametrized by a monad
data I m
    = Label (Free m String)     -- ^ label
    | Button { label_  :: Free m String
             , action_ :: Free m (Maybe (m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
             }  -- ^ button
    | Checkbox (Ref m Bool)         -- ^ checkbox
    | Combobox [String] (Ref m Int) -- ^ combo box
    | Entry (Ref m String)          -- ^ entry field
    | List ListLayout [I m]         -- ^ group interfaces into row or column
    | Notebook [(String, I m)]      -- ^ tabs
    | forall a . Eq a 
    => Cell { remember_content_   :: Bool       -- ^ remember the content or not? (info for the renderer)
            , underlying_value_   :: m a
            , dynamic_interface_  :: a -> I m
            }     -- ^ dynamic interface
    | Action (m (I m))              -- ^ do an action before giving the interface

data ListLayout
    = Horizontal | Vertical

