import Control.Monad
import Prelude hiding (id, (.))

import GUI.MLens.Gtk

import GUI.MLens.Gtk.Demos.Tri
import GUI.MLens.Gtk.Demos.IntListEditor
import GUI.MLens.Gtk.Demos.TEditor

main :: IO ()
main = runI $ Notebook
    [ (,) "IntListEditor" $ Action $ do
        state <- fileRef "intListEditorState.txt"
        settings <- fileRef "intListEditorSettings.txt"
        return $ intListEditor (justLens "" % state) (justLens "" % settings)
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]

justLens :: a -> Lens (Maybe a) a
justLens a = lens (maybe a id) (const . Just)


