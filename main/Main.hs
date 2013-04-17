{-# LANGUAGE RankNTypes #-}
import Control.Monad
import Control.Monad.Trans
import Prelude hiding ((.), id)

import Control.MLens.ExtRef.Pure
import GUI.MLens.Gtk.IO
import GUI.MLens.Gtk

import GUI.MLens.Gtk.Demos.Tri
import GUI.MLens.Gtk.Demos.IntListEditor
import GUI.MLens.Gtk.Demos.TEditor

realize :: (forall i . I (Ext i IO)) -> IO ()
realize e = runExt_ mapI e >>= runI

main :: IO ()
main = realize $ Notebook
    [ (,) "IntListEditor" $ Action $ do
        state <- lift $ liftM (mapMLens lift) $ join $ liftM memoMLens $ fileRef "intListEditorState.txt"
        settings <- lift $ liftM (mapMLens lift) $ join $ liftM memoMLens $ fileRef "intListEditorSettings.txt"
        return $ intListEditor state settings
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]

