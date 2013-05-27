module LGtk.Demos.Main
    ( main
    ) where

import Control.Monad
import Data.Maybe
import Prelude hiding (id, (.))

import LGtk

import LGtk.Demos.Tri
import LGtk.Demos.IntListEditor
import LGtk.Demos.TEditor

main :: IO ()
main = runWidget $ notebook
    [ (,) "Hello" $ label $ return "Hello World!"

    , (,) "Counters" $ notebook

        [ (,) "Unbounded" $ action $ do
            c <- newRef 0
            return $ vcat
                [ label $ liftM show $ readRef c
                , hcat
                    [ smartButton (return "+1") c $ return . (+1)
                    , smartButton (return "-1") c $ return . (+(-1))
                    ]
                ]

        , (,) "1..3" $ action $ do
            c <- newRef 1
            return $ vcat
                [ label $ liftM show $ readRef c
                , hcat
                    [ smartButton (return "+1") c $ return . min 3 . (+1)
                    , smartButton (return "-1") c $ return . max 1 . (+(-1))
                    ]
                ]

        , (,) "a..b" $ action $ do
            ab <- newRef (1, 3)
            let (a, b) = interval ab
            c <- counter 0 ab
            return $ vcat
                [ label $ liftM show $ readRef $ toRef c
                , hcat
                    [ smartButton' (return "+1") c (+1)
                    , smartButton' (return "-1") c (+(-1))
                    ]
                , hcat [ label $ return "min", entryShow a ]
                , hcat [ label $ return "max", entryShow b ]
                ]

        ]

    , (,) "Tabs" $ notebook

        [ (,) "TabSwitch" $ action $ do
            x <- newRef "a"
            let w = vcat [ label $ readRef x, entry x ]
            return $ notebook
                [ (,) "T1" w
                , (,) "T2" w
                ]

        ]

    , (,) "Async" $ action $ do
        ready <- newRef True
        delay <- newRef 1.0
        onChange False (readRef ready) $ \b -> return $ case b of
            True -> return ()
            False -> do
                d <- readRef' delay
                asyncWrite (ceiling $ 10^6 * d) (writeRef ready) True
        return $ vcat
            [ hcat [ entryShow delay, label $ return "sec" ]
            , button_ (readRef delay >>= \d -> return $ "Start " ++ show d ++ " sec computation") (readRef ready) $ writeRef ready False
            , label $ liftM (\b -> if b then "Ready." else "Computing...") $ readRef ready
            ]

    , (,) "Timer" $ action $ do
        t <- newRef 0
        onChange True (readRef t) $ \ti -> return $ asyncWrite (10^6) (writeRef t) (1 + ti) 
        return $ vcat
            [ label $ liftM show $ readRef t
            ]

    , (,) "System" $ notebook

        [ (,) "Args" $ action $ getArgs >>= \args -> return $ label $ return $ unlines args

        , (,) "ProgName" $ action $ getProgName >>= \args -> return $ label $ return args

        , (,) "Env" $ action $ do
            v <- newRef "HOME"
            return $ vcat
                [ entry v
                , label $ readRef v >>= liftM (maybe "Not in env." show) . lookupEnv 
                ]

        , (,) "Std I/O" $ let
            put = action $ do
                x <- newRef ""
                onChange False (readRef x) $ return . putStrLn_
                return $ hcat 
                    [ label $ return "putStrLn"
                    , entry x
                    ]
            get = action $ do
                ready <- newRef $ Just ""
                onChange False (liftM isJust $ readRef ready) $ \b -> 
                    return $ when (not b) $ getLine_ $ writeRef ready . Just
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

counter :: (EffRef m, Ord a) => a -> Ref m (a, a) -> m (EqRef (Ref m) a)
counter x ab = do
    c <- extRef ab (sndLens . fix) (x, (x, x))
    return $ EqRef c $ return $ fstLens . fix
  where
    fix = iso id $ \(x, ab@(a, b)) -> (min b $ max a x, ab)

interval :: (Reference r, Ord a) => r (a, a) -> (r a, r a)
interval ab = (lens fst set1 `lensMap` ab, lens snd set2 `lensMap` ab) where
    set1 a (_, b) = (min b a, b)
    set2 b (a, _) = (a, max a b)


