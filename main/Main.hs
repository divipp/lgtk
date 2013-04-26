import Control.Monad
import Prelude hiding (id, (.))

import Control.MLens.Unsafe
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
        state <- unsafeC $ fileRef "intListEditorState.txt"
        settings <- unsafeC $ fileRef "intListEditorSettings.txt"
        return $ intListEditor (justLens "" % state) (justLens "" % settings)
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]

justLens :: a -> Lens (Maybe a) a
justLens a = lens (maybe a id) (const . Just)


