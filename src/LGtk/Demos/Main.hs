module LGtk.Demos.Main
    ( main
    ) where

import Control.Monad
import Prelude hiding (id, (.))

import LGtk

import LGtk.Demos.Tri
import LGtk.Demos.IntListEditor
import LGtk.Demos.TEditor

main :: IO ()
main = runI $ notebook
    [ (,) "Hello" $ Label $ constEffect "Hello World!"

    , (,) "Counter" $ Action $ do
        c <- newRef 0
        return $ vcat
            [ Label $ rEffect $ liftM show $ readRef c
            , hcat
                [ smartButton (constEffect "+1") (return . (+1)) c
                , smartButton (constEffect "-1") (return . (+(-1))) c
                ]
            ]

    , (,) "Counter[1..3]" $ Action $ do
        c <- newRef 1
        return $ vcat
            [ Label $ rEffect $ liftM show $ readRef c
            , hcat
                [ smartButton (constEffect "+1") (return . min 3 . (+1)) c
                , smartButton (constEffect "-1") (return . max 1 . (+(-1))) c
                ]
            ]

    , (,) "Counter[a..b]" $ Action $ do
        a <- newRef 1
        b <- newRef 3
        let a' = joinRef $ do
                bv <- readRef b
                return $ lens id (const . min bv) % a
        let b' = joinRef $ do
                av <- readRef a
                return $ lens id (const . max av) % b
        return $ vcat
            [ counter a' b'
            , hcat [ Label $ constEffect "min", entry $ showLens % a' ]
            , hcat [ Label $ constEffect "max", entry $ showLens % b' ]
            ]

    , (,) "TabSwitch" $ Action $ do
        x <- newRef "a"
        let w = vcat [ Label $ rEffect $ readRef x, entry x ]
        return $ notebook
            [ (,) "T1" w
            , (,) "T2" w
            ]

    , (,) "IntListEditor" $ Action $ do
        state <- fileRef "intListEditorState.txt"
        settings <- fileRef "intListEditorSettings.txt"
        return $ intListEditor (justLens "" % state) (justLens "" % settings)
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]

justLens :: a -> Lens (Maybe a) a
justLens a = lens (maybe a id) (const . Just)

counter a b = Action $ do
    c <- newRef 0
    let c' = joinRef $ do
            av <- readRef a
            bv <- readRef b
            return $ iso (min bv . max av) id % c
    return $ vcat
        [ Label $ rEffect $ liftM show $ readRef c'
        , hcat
            [ smartButton (constEffect "+1") ((\x -> liftM (min x) $ readRef b) . (+1)) c'
            , smartButton (constEffect "-1") ((\x -> liftM (max x) $ readRef a) . (+(-1))) c'
            ]
        ]
