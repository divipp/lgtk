{-# LANGUAGE RankNTypes #-}
import Control.Monad
import Control.Monad.Trans
import Prelude hiding ((.), id)

import GUI.MLens.Gtk

import GUI.MLens.Gtk.Demos.Tri
import GUI.MLens.Gtk.Demos.IntListEditor
import GUI.MLens.Gtk.Demos.TEditor


{- |
We use @unsafeRunI@ only because we read from an write to a file.
It is considered unsafe usage, because the system do not guarantee that only this
program acesses the files at runtime.
-}
main :: IO ()
main = unsafeRunI $ Notebook
    [ (,) "IntListEditor" $ Action $ do
        state <- lift $ liftM (mapMLens lift) $ join $ liftM memoMLens $ fileRef "intListEditorState.txt"
        settings <- lift $ liftM (mapMLens lift) $ join $ liftM memoMLens $ fileRef "intListEditorSettings.txt"
        return $ intListEditor state settings
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]
{-
mainSafe :: IO ()
mainSafe = runI $ Notebook
    [ (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]
-}
