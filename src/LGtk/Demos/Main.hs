{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module LGtk.Demos.Main
    ( main
    ) where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Prelude hiding (id, (.))

import LGtk

import LGtk.Demos.Tri
import LGtk.Demos.IntListEditor
import LGtk.Demos.TEditor

main :: IO ()
main = runWidget $ notebook
    [ (,) "Hello" $ labelConst "Hello World!"

    , (,) "Counters" $ notebook

        [ (,) "Unbounded" $ action $ do
            c <- newRef 0
            return $ vcat
                [ label $ liftM show $ readRef c
                , hcat
                    [ smartButton (return "+1") (return . (+1)) c
                    , smartButton (return "-1") (return . (+(-1))) c
                    ]
                ]

        , (,) "1..3" $ action $ do
            c <- newRef 1
            return $ vcat
                [ label $ liftM show $ readRef c
                , hcat
                    [ smartButton (return "+1") (return . min 3 . (+1)) c
                    , smartButton (return "-1") (return . max 1 . (+(-1))) c
                    ]
                ]

        , (,) "a..b" $ action $ do
            a <- newRef 1
            b <- newRef 3
            let a' = joinRef $ do
                    bv <- readRef b
                    return $ lens id (const . min bv) `lensMap` a
            let b' = joinRef $ do
                    av <- readRef a
                    return $ lens id (const . max av) `lensMap` b
            return $ vcat
                [ counter a' b'
                , hcat [ labelConst "min", entry $ showLens `lensMap` a' ]
                , hcat [ labelConst "max", entry $ showLens `lensMap` b' ]
                ]

        ]

    , (,) "TabSwitch" $ action $ do
        x <- newRef "a"
        let w = vcat [ label $ readRef x, entry x ]
        return $ notebook
            [ (,) "T1" w
            , (,) "T2" w
            ]

    , (,) "Async" $ action $ do
        ready <- newRef True
        delay <- newRef 1.0
        onChange (readRef ready) $ \b -> case b of
            True -> return ()
            False -> do
                d <- readRef' delay
                asyncWrite ready True $ ceiling $ 10^6 * d
        return $ vcat
            [ hcat [ entry $ showLens `lensMap` delay, labelConst "sec" ]
            , button_ (readRef delay >>= \d -> return $ "Start " ++ show d ++ " sec computation") (readRef ready) $ writeRef ready False
            , label $ liftM (\b -> if b then "Ready." else "Computing...") $ readRef ready
            ]

    , (,) "Timer" $ action $ do
        t <- newRef 0
        onChange (readRef t) $ \ti -> asyncWrite t (1 + ti) (10^6)
        return $ vcat
            [ label $ readRef $ showLens `lensMap` t
            ]

    , (,) "System" $ notebook

        [ (,) "Args" $ action $ getArgs >>= \args -> return $ labelConst $ unlines args

        , (,) "ProgName" $ action $ getProgName >>= \args -> return $ labelConst args

        , (,) "Env" $ action $ do
            v <- newRef "HOME"
            return $ vcat
                [ entry v
                , label $ readRef v >>= liftM (maybe "Not in env." show) . lookupEnv 
                ]

        , (,) "Std I/O" $ let
            put = action $ do
                x <- newRef ""
                onChange (readRef x) putStrLn_
                return $ hcat 
                    [ labelConst "putStrLn"
                    , entry x
                    ]
            get = action $ do
                ready <- newRef $ Just ""
                onChange (liftM isJust $ readRef ready) $ \b -> 
                    when (not b) $ getLine_ $ writeRef ready . Just
                return $ hcat 
                    [ button_ (return "getLine") (liftM isJust $ readRef ready) $ writeRef ready Nothing
                    , label $ liftM (maybe "<<<waiting for input>>>" id) $ readRef ready
                    ]
           in vcat [ put, put, put, get, get, get ]
        ]

    , (,) "IntListEditor" $ action $ do
        state <- fileRef "intListEditorState.txt"
        settings <- fileRef "intListEditorSettings.txt"
        return $ intListEditor (justLens "" `lensMap` state) (justLens "" `lensMap` settings)
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3

    ]

justLens :: a -> Lens (Maybe a) a
justLens a = lens (maybe a id) (const . Just)

counter a b = action $ do
    c <- newRef 0
    let c' = joinRef $ do
            av <- readRef a
            bv <- readRef b
            return $ iso (min bv . max av) id `lensMap` c
    return $ vcat
        [ label $ liftM show $ readRef c'
        , hcat
            [ smartButton (return "+1") ((\x -> liftM (min x) $ readRef b) . (+1)) c'
            , smartButton (return "-1") ((\x -> liftM (max x) $ readRef a) . (+(-1))) c'
            ]
        ]

