import Control.Monad
import Control.Monad.Trans

import Control.MLens.Unsafe (fileRef)
import GUI.MLens.Gtk

import GUI.MLens.Gtk.Demos.Tri
import GUI.MLens.Gtk.Demos.IntListEditor
import GUI.MLens.Gtk.Demos.TEditor


{- |
We use @unsafeRunI@ only because we read from and write to a file.
It is considered unsafe usage, because the system do not guarantee that only this
program acesses the files at runtime.
-}
main :: IO ()
main = unsafeRunI $ Notebook
    [ (,) "IntListEditor" $ Action $ do
        state <- mapC lift $ liftM (mapRef lift) $ join $ liftM memoRef $ fileRef "intListEditorState.txt"
        settings <- mapC lift $ liftM (mapRef lift) $ join $ liftM memoRef $ fileRef "intListEditorSettings.txt"
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
