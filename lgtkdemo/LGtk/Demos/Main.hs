{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Demos.Main
    ( main
    ) where

import Numeric
import Data.Maybe (isJust)
import Control.Lens hiding ((#))
import Control.Monad
import Control.Monad.Fix
import Diagrams.Prelude hiding (vcat, hcat, interval, tri)

import LGtk

import LGtk.Demos.Tri
import LGtk.Demos.IntListEditor
import LGtk.Demos.TEditor
import LGtk.Demos.Maze

main :: IO ()
main = runWidget mainWidget

mainWidget :: Widget
mainWidget = notebook
    [ (,) "Simple" $ notebook

--      , (,) "Hello" $ label $ pure "Hello World!"

        [ (,) "Counters" $ notebook

            [ (,) "Unbounded" $ do
                c <- newEqRef (0 :: Int)
                vcat
                    [ label $ fmap show $ readRef c
                    , hcat
                        [ smartButton (pure "+1") c (+1)
                        , smartButton (pure "-1") c (+(-1))
                        ]
                    ]

            , (,) "1..3" $ do
                c <- newEqRef (1 :: Int)
                vcat
                    [ label $ fmap show $ readRef c
                    , hcat
                        [ smartButton (pure "+1") c $ min 3 . (+1)
                        , smartButton (pure "-1") c $ max 1 . (+(-1))
                        ]
                    ]

            , (,) "a..b" $ do
                ab <- newRef (1 :: Int, 3)
                let (a, b) = interval ab
                c <- counter 0 ab
                vcat
                    [ label $ fmap show $ readRef c
                    , hcat
                        [ smartButton (pure "+1") c (+1)
                        , smartButton (pure "-1") c (+(-1))
                        ]
                    , hcat [ label $ pure "min", entryShow a ]
                    , hcat [ label $ pure "max", entryShow b ]
                    ]

            ]

{-
        , (,) "Buttons" $ do
            x <- newRef (0 :: Int)
            let is = [0 :: Double, 0.5, 1]
                colorlist = tail $ liftA3 sRGB is is is
                f n = colorlist !! (n `mod` length colorlist)
            button__ (pure "Push") (pure True) (fmap f $ readRef x) $ modRef x (+1)

        , (,) "Tabs" $ notebook

            [ (,) "TabSwitch" $ do
                x <- newRef "a"
                let w = vcat [ label $ readRef x, entry x ]
                notebook
                    [ (,) "T1" w
                    , (,) "T2" w
                    ]

            ]
-}

        , (,) "Tri" tri

        , (,) "T-Editor" $ notebook

            [ (,) "Version 1" $ do
                t <- newRef $ iterate (Node Leaf) Leaf !! 10
                hcat
                    [ canvas 200 200 20 (const $ pure ()) Nothing (readRef t) $
                        \x -> tPic 0 x # lwL 0.05 # value () # translate (r2 (0,10))
                    , tEditor3 t
                    ]

            , (,) "Version 2" tEditor1
            ]

        , (,) "Notebook" $ notebook

            [ (,) "Version 2" $ do
                buttons <- newRef ("",[])
                let ctrl = entry $ lens fst (\(_,xs) x -> ("",x:xs)) `lensMap` buttons
                    h b = do
                        q <- extRef b listLens (False, ("", []))
                        cell (fmap fst $ readRef q) $ \bb -> case bb of
                            False -> empty
                            _ -> do
                                vcat $ reverse
                                    [ h $ _2 . _2 `lensMap` q
                                    , hcat
                                        [ button (pure "Del") $ pure $ Just $ modRef b tail
                                        , label $ readRef $ _2 . _1 `lensMap` q
                                        ]
                                    ]
                vcat $ [ctrl, h $ _2 `lensMap` buttons]

            , (,) "Version 1" $ do
                buttons <- newRef ("",[])
                let h i b = hcat
                       [ label $ pure b
                       , button (pure "Del") $ pure $ Just $ modRef (_2 `lensMap` buttons) $ \l -> take i l ++ drop (i+1) l
                       ]
                    set (a,xs) x
                        | a /= x = ("",x:xs)
                        | otherwise = (a,xs)
                vcat
                    [ entry $ lens fst set `lensMap` buttons
                    , cell (fmap snd $ readRef buttons) $ vcat . zipWith h [0..]
                    ]

            ]

        ]

    , (,) "Canvas" $ notebook

        [ (,) "NotReactive" $ notebook

            [ (,) "Dynamic" $ do

                r <- newRef (3 :: Double)
                vcat
                    [ canvas 200 200 12 (const $ pure ()) Nothing (readRef r) $
                        \x -> circle x # lwL 0.05 # fc blue # value ()
                    , hcat
                        [ hscale 0.1 5 0.05 r
                        , label (fmap (("radius: " ++) . ($ "") . showFFloat (Just 2)) $ readRef r)
                        ]
                    ]

            , (,) "Animation" $ do

                fps <- newRef (50 :: Double)
                speed <- newRef (1 :: Double)
                phase <- newRef (0 :: Double)
                t <- newRef 0
                _ <- onChangeEq (readRef phase) $ \x -> do
                    s <- readRef speed
                    f <- readRef fps
                    asyncWrite (round $ 1000000 / f) $ writeRef phase (x + 2 * pi * s / f)
                vcat
                    [ canvas 200 200 10 (const $ pure ()) Nothing (liftA2 (,) (readRef t) (readRef phase)) $
                        \(t,x) -> (case t of
                            0 -> circle (2 + 1.5*sin x)
                            1 -> circle 1 # translate (r2 (3,0)) # rotate ((-x) @@ rad)
                            2 -> rect 6 6 # rotate ((-x) @@ rad)
                            3 -> mconcat [circle (i'/10) # translate (r2 (i'/3, 0) # rotate ((i') @@ rad)) | i<-[1 :: Int ..10], let i' = fromIntegral i] # rotate ((-x) @@ rad)
                            4 -> mconcat [circle (i'/10) # translate (r2 (i'/3, 0) # rotate ((x/i') @@ rad)) | i<-[1 :: Int ..10], let i' = fromIntegral i]
                            ) # lwL 0.05 # fc blue # value ()
                    , combobox ["Pulse","Rotate","Rotate2","Spiral","Spiral2"] t
                    , hcat
                        [ hscale 0.1 5 0.1 speed
                        , label (fmap (("freq: " ++) . ($ "") . showFFloat (Just 2)) $ readRef speed)
                        ]
                    , hcat
                        [ hscale 1 100 1 fps
                        , label (fmap (("fps: " ++) . ($ "") . showFFloat (Just 2)) $ readRef fps)
                        ]
                    ]

            ]

        , (,) "Reactive" $ notebook

            [ (,) "ColorChange" $ do
                phase <- newRef (0 :: Double)
                col <- newRef True
                _ <- onChangeEq (readRef phase) $ \x -> do
                    let s = 0.5 :: Double
                    let f = 50 :: Double
                    asyncWrite (round $ 1000000 / f) $ writeRef phase (x + 2 * pi * s / f)
                let handler (Click (MousePos _ l), _) = when (not $ null l) $ modRef col not
                    handler _ = pure ()
                vcat
                    [ canvas 200 200 10 handler Nothing (liftA2 (,) (readRef col) (readRef phase)) $
                        \(c,x) -> circle 1 # translate (r2 (3,0)) # rotate ((-x) @@ rad) # lwL 0.05 # fc (if c then blue else red) # value [()]
                    , label $ pure "Click on the circle to change color."
                    ]

            , (,) "Enlarge" $ do
                phase <- newRef (0 :: Double)
                col <- newRef 1
                _ <- onChangeEq (readRef phase) $ \x -> do
                    let s = 0.5 :: Double
                    let f = 50 :: Double
                    asyncWrite (round $ 1000000 / f) $ do
                        writeRef phase (x + 2 * pi * s / f)
                        modRef col $ max 1 . (+(- 5/f))
                let handler (Click (MousePos _ l), _) = when (not $ null l) $ modRef col (+1)
                    handler _ = pure ()
                vcat
                    [ canvas 200 200 10 handler Nothing (liftA2 (,) (readRef col) (readRef phase)) $
                        \(c,x) -> circle c # translate (r2 (3,0)) # rotate ((-x) @@ rad) # lwL 0.05 # fc blue # value [()]
                    , label $ pure "Click on the circle to temporarily enlarge it."
                    ]

                , (,) "Chooser" $ do
                i <- newRef (0 :: Int, 0 :: Rational)
                let i1 = _1 `lensMap` i
                    i2 = _2 `lensMap` i
                _ <- onChangeEq (readRef i) $ \(i,d) -> do
                    let dd = fromIntegral i - d
                    if dd == 0
                      then pure ()
                      else do
                        let s = 2 :: Rational
                        let f = 25 :: Rational
                        asyncWrite (round $ 1000000 / f) $ do
                            writeRef i2 $ d + signum dd * min (abs dd) (s / f)
                let keyh (SimpleKey Key'Left)  = modRef i1 pred >> pure True
                    keyh (SimpleKey Key'Right) = modRef i1 succ >> pure True
                    keyh _ = pure False
                vcat
                    [ canvas 200 200 10 (const $ pure ()) (Just keyh) (readRef i2) $
                        \d -> text "12345" # translate (r2 (realToFrac d, 0)) # scale 2 # value ()
                    , label $ fmap show $ readRef i1
                    ]

            ]

        , (,) "InCanvas" $ notebook

            [ (,) "Widgets" $ inCanvasExample

            , (,) "Recursive" $ inCanvas 800 600 30 mainWidget

            ]

        ]

    , (,) "System" $ notebook

    {-
        , (,) "Accumulator" $ do
            x <- newRef (0 :: Integer)
            y <- onChangeAcc (readRef x) 0 (const 0) $ \x _ y -> Left $ pure $ x+y
            hcat
                [ entryShow x
                , label $ fmap show y
                ]
    -}
        [ (,) "Async" $ do
            ready <- newRef True
            delay <- newRef (1.0 :: Double)
            _ <- onChangeEq (readRef ready) $ \b -> case b of
                True -> pure ()
                False -> do
                    d <- readRef delay
                    asyncWrite (ceiling $ 1000000 * d) $ writeRef ready True
            vcat
                [ hcat [ entryShow delay, label $ pure "sec" ]
                , primButton (flip fmap (readRef delay) $ \d -> "Start " ++ show d ++ " sec computation")
                          (readRef ready)
                          Nothing
                          (writeRef ready False)
                , label $ fmap (\b -> if b then "Ready." else "Computing...") $ readRef ready
                ]

        , (,) "Timer" $ do
            t <- newRef (0 :: Int)
            _ <- onChangeEq (readRef t) $ \ti -> asyncWrite 1000000 $ writeRef t $ 1 + ti
            vcat
                [ label $ fmap show $ readRef t
                ]

        , (,) "System" $ notebook

            [ (,) "Args" $ getArgs >>= \args -> label $ pure $ unlines args

            , (,) "ProgName" $ getProgName >>= \args -> label $ pure args

            , (,) "Env" $ do
                v <- newRef "HOME"
                lv <- onChangeEq (readRef v) $ fmap (maybe "Not in env." show) . lookupEnv
                vcat
                    [ entry v
                    , label lv
                    ]

            , (,) "Std I/O" $ let
                put = do
                    x <- newRef Nothing
                    _ <- onChangeEq (readRef x) $ maybe (pure ()) putStrLn_
                    hcat 
                        [ label $ pure "putStrLn"
                        , entry $ iso (maybe "" id) Just `lensMap` x
                        ]
                get = do
                    ready <- newRef $ Just ""
                    _ <- onChangeEq (fmap isJust $ readRef ready) $ \b -> 
                        when (not b) $ getLine_ $ writeRef ready . Just
                    hcat 
                        [ primButton (pure "getLine") (fmap isJust $ readRef ready) Nothing $ writeRef ready Nothing
                        , label $ fmap (maybe "<<<waiting for input>>>" id) $ readRef ready
                        ]
               in vcat [ put, put, put, get, get, get ]
            ]
        ]

    , (,) "Complex" $ notebook

        [ (,) "ListEditor" $ do
            state <- fileRef "intListEditorState.txt"
            list <- extRef (justLens "" `lensMap` state) showLens []
            settings <- fileRef "intListEditorSettings.txt"
            range <- extRef (justLens "" `lensMap` settings) showLens True
            intListEditor (0 :: Integer, True) 15 list range

        , (,) "Maze" $ mazeGame

        ]

{-
    , (,) "Csaba" $ notebook

        [ (,) "#1" $ do
            name <- newRef "None"
            buttons <- newRef []
            let ctrl = hcat
                    [ label $ readRef name
                    , button (pure "Add") $ pure $ Just $ do
                        l <- readRef buttons
                        let n = "Button #" ++ (show . length $ l)
                        writeRef buttons $ n:l
                    ]
                f n = vcat $ map g n 
                g n = button (pure n) (pure . Just $ writeRef name n)
            vcat $ [ctrl, cell (readRef buttons) f]

        , (,) "#2" $ do
            name <- newRef "None"
            buttons <- newRef []
            let ctrl = hcat
                    [ label $ readRef name
                    , button (pure "Add") $ pure $ Just $ do
                        l <- readRef buttons
                        let n = "Button #" ++ (show . length $ l)
                        writeRef buttons $ l ++ [n]
                    ]
                h b = do
                    q <- extRef b listLens (False, ("", []))
                    cell (fmap fst $ readRef q) $ \b -> case b of
                        False -> empty
                        _ -> do
                            na <- readRef $ _2 . _1 `lensMap` q
                            vcat $ reverse
                                [ h $ _2 . _2 `lensMap` q
                                , hcat [ button (pure na) $ pure $ Just $ writeRef name na, entry $ _2 . _1 `lensMap` q ]
                                ]
            vcat $ [ctrl, h buttons]

        ]
-}    
    ]

tPic :: Int -> T -> Dia Any
tPic _ Leaf = circle 0.5 # fc blue
tPic i (Node a b) = tPic (i+1) a # translate (r2 (-w,-2))
               <> tPic (i+1) b # translate (r2 (w,-1.8))
               <> fromVertices [p2 (-w, -2), p2 (0,0), p2 (w,-1.8)]
  where w = 3 * 0.7 ^ i

justLens :: a -> Lens' (Maybe a) a
justLens a = lens (maybe a id) (flip $ const . Just)

counter :: forall a . (Ord a) => a -> SubState (a, a) -> Create (SubStateEq a)
counter x ab = do
    c <- extRef ab (fix . _2) (x, (x, x))
    pure $ fix . _1 `lensMap` toEqRef c
  where
    fix :: Lens' (a, (a,a)) (a, (a,a))
    fix = lens id $ \_ (x, ab@(a, b)) -> (min b $ max a x, ab)

interval :: (RefClass r, Ord a) => RefSimple r (a, a) -> (RefSimple r a, RefSimple r a)
interval ab = (lens fst set1 `lensMap` ab, lens snd set2 `lensMap` ab) where
    set1 (_, b) a = (min b a, b)
    set2 (a, _) b = (a, max a b)


----------------------------------------------------------------------------

inCanvasExample = do
    t <- newRef $ iterate (Node Leaf) Leaf !! 5
    i <- newRef (0 :: Int)
    j <- newRef 0
    s <- newRef "x"
    s' <- newRef "y"
    let x = vcat
            [ hcat
                [ vcat
                    [ hcat
                        [ label $ fmap (\i -> show i ++ "hello") $ readRef i
                        , primButton (pure "+1") (pure True) Nothing $ modRef i (+1)
                        ]
                    , hcat
                        [ entry s
                        , entry s
                        ]
                    , hcat
                        [ entry s'
                        , entry s'
                        ]
                    ]
                , combobox ["Hello","World","!"] j
                ]
            , tEditor3 t
            ]

    hcat [ inCanvas 200 300 15 $ vcat [x, inCanvas 100 100 15 x], x]



