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
import Diagrams.Prelude hiding (atTime, Point, Start, adjust, value, interval, tri)
import qualified Diagrams.Prelude as D

import LGtk

import LGtk.Demos.Tri
import LGtk.Demos.IntListEditor
import LGtk.Demos.TEditor
import LGtk.Demos.Maze

import qualified LGtk.Demos.SevenGuis.Circles as Seven
import qualified LGtk.Demos.SevenGuis.Counter as Seven
import qualified LGtk.Demos.SevenGuis.Temperature as Seven

main :: IO ()
main = runWidget mainWidget

mainWidget :: Widget
mainWidget = notebook
    [ (,) "Simple" $ notebook

--      , (,) "Hello" $ label $ pure "Hello World!"

        [ (,) "Counters" $ notebook

            [ (,) "Unbounded" $ do
                c <- fmap toEqRef $ newRef (0 :: Int)
                vertically
                    [ label $ fmap show $ value c
                    , horizontally
                        [ smartButton (pure "+1") c (+1)
                        , smartButton (pure "-1") c (+(-1))
                        ]
                    ]

            , (,) "1..3" $ do
                c <- fmap toEqRef $ newRef (1 :: Int)
                vertically
                    [ label $ fmap show $ value c
                    , horizontally
                        [ smartButton (pure "+1") c $ min 3 . (+1)
                        , smartButton (pure "-1") c $ max 1 . (+(-1))
                        ]
                    ]

            , (,) "a..b" $ do
                ab <- newRef (1 :: Int, 3)
                let (a, b) = interval ab
                c <- counter 0 ab
                vertically
                    [ label $ fmap show $ value c
                    , horizontally
                        [ smartButton (pure "+1") c (+1)
                        , smartButton (pure "-1") c (+(-1))
                        ]
                    , horizontally [ label $ pure "min", entryShow a ]
                    , horizontally [ label $ pure "max", entryShow b ]
                    ]

            ]

{-
        , (,) "Buttons" $ do
            x <- newRef (0 :: Int)
            let is = [0 :: Double, 0.5, 1]
                colorlist = tail $ liftA3 sRGB is is is
                f n = colorlist !! (n `mod` length colorlist)
            button__ (pure "Push") (pure True) (fmap f $ value x) $ adjust x (+1)

        , (,) "Tabs" $ notebook

            [ (,) "TabSwitch" $ do
                x <- newRef "a"
                let w = vertically [ label $ value x, entry x ]
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
                horizontally
                    [ canvas 200 200 20 (const $ pure ()) Nothing (value t) $
                        \x -> tPic 0 x # lwL 0.05 # D.value () # translate (r2 (0,10))
                    , tEditor3 t
                    ]

            , (,) "Version 2" tEditor1
            ]

        , (,) "Notebook" $ notebook

            [ (,) "Version 2" $ do
                buttons <- newRef ("",[])
                let ctrl = entry $ lens fst (\(_,xs) x -> ("",x:xs)) `lensMap` buttons
                    h b = do
                        q <- extendRef b listLens (False, ("", []))
                        cell (fmap fst $ value q) $ \bb -> case bb of
                            False -> emptyWidget
                            _ -> do
                                vertically $ reverse
                                    [ h $ _2 . _2 `lensMap` q
                                    , horizontally
                                        [ button (pure "Del") $ pure $ Just $ adjust b tail
                                        , label $ value $ _2 . _1 `lensMap` q
                                        ]
                                    ]
                vertically $ [ctrl, h $ _2 `lensMap` buttons]

            , (,) "Version 1" $ do
                buttons <- newRef ("",[])
                let h i b = horizontally
                       [ label $ pure b
                       , button (pure "Del") $ pure $ Just $ adjust (_2 `lensMap` buttons) $ \l -> take i l ++ drop (i+1) l
                       ]
                    set (a,xs) x
                        | a /= x = ("",x:xs)
                        | otherwise = (a,xs)
                vertically
                    [ entry $ lens fst set `lensMap` buttons
                    , cell (fmap snd $ value buttons) $ vertically . zipWith h [0..]
                    ]

            ]

        ]

    , (,) "Canvas" $ notebook

        [ (,) "NotReactive" $ notebook

            [ (,) "Dynamic" $ do

                r <- newRef (3 :: Double)
                vertically
                    [ canvas 200 200 12 (const $ pure ()) Nothing (value r) $
                        \x -> circle x # lwL 0.05 # fc blue # D.value ()
                    , horizontally
                        [ hscale 0.1 5 0.05 r
                        , label (fmap (("radius: " ++) . ($ "") . showFFloat (Just 2)) $ value r)
                        ]
                    ]

            , (,) "Animation" $ do

                fps <- newRef (50 :: Double)
                speed <- newRef (1 :: Double)
                phase <- newRef (0 :: Double)
                t <- newRef 0
                _ <- onChangeEq (value phase) $ \x -> do
                    s <- readerToCreator $ value speed
                    f <- readerToCreator $ value fps
                    asyncWrite (round $ 1000000 / f) $ writeRef phase (x + 2 * pi * s / f)
                vertically
                    [ canvas 200 200 10 (const $ pure ()) Nothing (liftA2 (,) (value t) (value phase)) $
                        \(t,x) -> (case t of
                            0 -> circle (2 + 1.5*sin x)
                            1 -> circle 1 # translate (r2 (3,0)) # rotate ((-x) @@ rad)
                            2 -> rect 6 6 # rotate ((-x) @@ rad)
                            3 -> mconcat [circle (i'/10) # translate (r2 (i'/3, 0) # rotate ((i') @@ rad)) | i<-[1 :: Int ..10], let i' = fromIntegral i] # rotate ((-x) @@ rad)
                            4 -> mconcat [circle (i'/10) # translate (r2 (i'/3, 0) # rotate ((x/i') @@ rad)) | i<-[1 :: Int ..10], let i' = fromIntegral i]
                            ) # lwL 0.05 # fc blue # D.value ()
                    , combobox ["Pulse","Rotate","Rotate2","Spiral","Spiral2"] t
                    , horizontally
                        [ hscale 0.1 5 0.1 speed
                        , label (fmap (("freq: " ++) . ($ "") . showFFloat (Just 2)) $ value speed)
                        ]
                    , horizontally
                        [ hscale 1 100 1 fps
                        , label (fmap (("fps: " ++) . ($ "") . showFFloat (Just 2)) $ value fps)
                        ]
                    ]

            ]

        , (,) "Reactive" $ notebook

            [ (,) "ColorChange" $ do
                phase <- newRef (0 :: Double)
                col <- newRef True
                _ <- onChangeEq (value phase) $ \x -> do
                    let s = 0.5 :: Double
                    let f = 50 :: Double
                    asyncWrite (round $ 1000000 / f) $ writeRef phase (x + 2 * pi * s / f)
                let handler (Click (MousePos _ l), _) = when (not $ null l) $ adjust col not
                    handler _ = pure ()
                vertically
                    [ canvas 200 200 10 handler Nothing (liftA2 (,) (value col) (value phase)) $
                        \(c,x) -> circle 1 # translate (r2 (3,0)) # rotate ((-x) @@ rad) # lwL 0.05 # fc (if c then blue else red) # D.value [()]
                    , label $ pure "Click on the circle to change color."
                    ]

            , (,) "Enlarge" $ do
                phase <- newRef (0 :: Double)
                col <- newRef 1
                _ <- onChangeEq (value phase) $ \x -> do
                    let s = 0.5 :: Double
                    let f = 50 :: Double
                    asyncWrite (round $ 1000000 / f) $ do
                        writeRef phase (x + 2 * pi * s / f)
                        adjust col $ max 1 . (+(- 5/f))
                let handler (Click (MousePos _ l), _) = when (not $ null l) $ adjust col (+1)
                    handler _ = pure ()
                vertically
                    [ canvas 200 200 10 handler Nothing (liftA2 (,) (value col) (value phase)) $
                        \(c,x) -> circle c # translate (r2 (3,0)) # rotate ((-x) @@ rad) # lwL 0.05 # fc blue # D.value [()]
                    , label $ pure "Click on the circle to temporarily enlarge it."
                    ]

                , (,) "Chooser" $ do
                i <- newRef (0 :: Int, 0 :: Rational)
                let i1 = _1 `lensMap` i
                    i2 = _2 `lensMap` i
                _ <- onChangeEq (value i) $ \(i,d) -> do
                    let dd = fromIntegral i - d
                    if dd == 0
                      then pure ()
                      else do
                        let s = 2 :: Rational
                        let f = 25 :: Rational
                        asyncWrite (round $ 1000000 / f) $ do
                            writeRef i2 $ d + signum dd * min (abs dd) (s / f)
                let keyh (SimpleKey Key'Left)  = adjust i1 pred >> pure True
                    keyh (SimpleKey Key'Right) = adjust i1 succ >> pure True
                    keyh _ = pure False
                vertically
                    [ canvas 200 200 10 (const $ pure ()) (Just keyh) (value i2) $
                        \d -> text "12345" # translate (r2 (realToFrac d, 0)) # scale 2 # D.value ()
                    , label $ fmap show $ value i1
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
            y <- onChangeAcc (value x) 0 (const 0) $ \x _ y -> Left $ pure $ x+y
            horizontally
                [ entryShow x
                , label $ fmap show y
                ]
    -}
        [ (,) "Async" $ do
            ready <- newRef True
            delay <- newRef (1.0 :: Double)
            _ <- onChangeEq (value ready) $ \b -> case b of
                True -> pure ()
                False -> do
                    d <- readerToCreator $ value delay
                    asyncWrite (ceiling $ 1000000 * d) $ writeRef ready True
            vertically
                [ horizontally [ entryShow delay, label $ pure "sec" ]
                , primButton (flip fmap (value delay) $ \d -> "Start " ++ show d ++ " sec computation")
                          (value ready)
                          Nothing
                          (writeRef ready False)
                , label $ fmap (\b -> if b then "Ready." else "Computing...") $ value ready
                ]

        , (,) "Timer" $ do
            rt <- newRef =<< time
            _ <- onChangeEq (value rt) $ \ti -> do
                putStrLn_ $ show ti
                atTime (addUTCTime 1 ti) $ writeRef rt (addUTCTime 1 ti)
            vertically
                [ label $ fmap show $ value rt
                ]

        , (,) "System" $ notebook

            [ (,) "Args" $ getArgs >>= \args -> label $ pure $ unlines args

            , (,) "ProgName" $ getProgName >>= \args -> label $ pure args

            , (,) "Env" $ do
                v <- newRef "HOME"
                lv <- onChangeEq (value v) $ fmap (maybe "Not in env." show) . lookupEnv
                vertically
                    [ entry v
                    , label lv
                    ]

            , (,) "Std I/O" $ let
                put = do
                    x <- newRef Nothing
                    _ <- onChangeEq (value x) $ maybe (pure ()) putStrLn_
                    horizontally
                        [ label $ pure "putStrLn"
                        , entry $ iso (maybe "" id) Just `lensMap` x
                        ]
                get = do
                    ready <- newRef $ Just ""
                    _ <- onChangeEq (fmap isJust $ value ready) $ \b ->
                        when (not b) $ getLine_ $ writeRef ready . Just
                    horizontally
                        [ primButton (pure "getLine") (fmap isJust $ value ready) Nothing $ writeRef ready Nothing
                        , label $ fmap (maybe "<<<waiting for input>>>" id) $ value ready
                        ]
               in vertically [ put, put, put, get, get, get ]
            ]
        ]

    , (,) "Complex" $ notebook

        [ (,) "ListEditor" $ do
            state <- fileRef "intListEditorState.txt"
            -- NOTE: if there was some kind of 'prismRef', then _Show
            -- could be used instead of showLens.
            list <- extendRef (justLens "" `lensMap` state) showLens []
            settings <- fileRef "intListEditorSettings.txt"
            range <- extendRef (justLens "" `lensMap` settings) showLens True
            intListEditor (0 :: Integer, True) 15 list range

        , (,) "Maze" $ mazeGame

        ]

    , (,) "7guis" $ notebook
        [ (,) "#1 - Counter"               Seven.counter
        , (,) "#2 - Temperature Converter" Seven.temperatureConverter
        , (,) "#6 - Circle Drawer"         Seven.circleDrawer
        ]

    ]

tPic :: Int -> T -> Dia Any
tPic _ Leaf = circle 0.5 # fc blue
tPic i (Node a b) = tPic (i+1) a # translate (r2 (-w,-2))
               <> tPic (i+1) b # translate (r2 (w,-1.8))
               <> fromVertices [p2 (-w, -2), p2 (0,0), p2 (w,-1.8)]
  where w = 3 * 0.7 ^ i

justLens :: a -> Lens' (Maybe a) a
justLens a = lens (maybe a id) (flip $ const . Just)

counter :: forall a . (Ord a) => a -> Ref (a, a) -> RefCreator (EqRef a)
counter x ab = do
    c <- extendRef ab (fix . _2) (x, (x, x))
    pure $ fix . _1 `lensMap` toEqRef c
  where
    fix :: Lens' (a, (a,a)) (a, (a,a))
    fix = lens id $ \_ (x, ab@(a, b)) -> (min b $ max a x, ab)

interval :: (RefClass r, Ord a) => r (a, a) -> (r a, r a)
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
    let x = vertically
            [ horizontally
                [ vertically
                    [ horizontally
                        [ label $ fmap (\i -> show i ++ "hello") $ value i
                        , primButton (pure "+1") (pure True) Nothing $ adjust i (+1)
                        ]
                    , horizontally
                        [ entry s
                        , entry s
                        ]
                    , horizontally
                        [ entry s'
                        , entry s'
                        ]
                    ]
                , combobox ["Hello","World","!"] j
                ]
            , tEditor3 t
            ]

    horizontally [ inCanvas 200 300 15 $ vertically [x, inCanvas 100 100 15 x], x]
