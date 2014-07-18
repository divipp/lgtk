{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LGtk.Demos.Maze where

import Control.Monad.State
import Data.List
import Data.Array
import qualified Data.Set as S
import System.Random
import Diagrams.Prelude hiding (Point, Start)
import Control.Lens hiding ((#))

import LGtk

import LGtk.Demos.Maze.Types
import qualified LGtk.Demos.MazeGen as Maze1
import qualified LGtk.Demos.Maze.Maze as Maze2

---------------------------------- Game state

data GameState
    = Start
    | Explore Point
    | Fail
    | Success

instance Show GameState where
    show Success = "Congratulation!"
    show Fail = "Failure, try again!"
    show Start = "Move the pointer to the filled circle"
    show (Explore _) = "Reach the bottom left corner"

------------------------ maze drawing

drawMaze :: (Maze, S.Set Point, Maybe Point) -> Dia [Point]
drawMaze (maze, hi, pos) =
    (  mconcat (map drawCell $ assocs maze) # centerXY
    <> rect (fromIntegral $ x2-x1+1) (fromIntegral $ y2-y1+1) # lwL wallwidth # fc (sRGB 0.95 0.95 0.95) # value []
    ) # scale (1 / fromIntegral (max (x2-x1+1) (y2-y1+1)))
  where
    drawCell (p@(i,j), C cs) =
            (   (if b then mconcat (map drawWall $ complement cs) # lwL wallwidth # value [] else mempty)
            <>  (if Just p == pos then circ # fc blue else mempty)
            <>  (if p == q2 || p == q1 then circ else mempty)
            <>  rect 1 1 # lwL 0 # (if b then fc yellow else id) # value [p]
            )   # translate (r2 (fromIntegral i, fromIntegral j))
        where b = S.member p hi
    circ = circle 0.35 # lwL 0.005 # value []
    wallwidth = 0.02

    drawWall E = fromVertices [p2 (-d, d), p2 (d, d)]
    drawWall S = fromVertices [p2 (d, -d), p2 (d, d)]
    drawWall W = fromVertices [p2 (-d, -d), p2 (d, -d)]
    drawWall N = fromVertices [p2 (-d, -d), p2 (-d, d)]

    d = 0.5

    (q1@(x1,y1), q2@(x2,y2)) = bounds maze

complement :: [Cardinal] -> [Cardinal]
complement = f [N,E,S,W] . sort
  where
    f (x:xs) (y:ys) | x == y = f xs ys
    f (x:xs) ys = x: f xs ys
    f [] _ = []

----------------------- game logic

-- | check whether a move does not hit the wall
isOk :: Maze -> Point -> Point -> Bool
isOk maze p q = p == q || maybe False (`elem` unC (maze ! p)) (dir p q)
  where
    unC (C xs) = xs

    dir (a,b) (c,d)
        | a == c && b == d + 1 = Just W
        | a == c && b == d - 1 = Just E
        | a == c + 1 && b == d = Just N
        | a == c - 1 && b == d = Just S
        | otherwise = Nothing

checkBounds ((a,b),(c,d)) (e,f)
    | a <= e && e <= c && b <= f && f <= d = Just (e,f)
    | otherwise = Nothing

gameLogic b maze p (s, st) = case st of
    Start | p == snd (bounds maze) -> commit
    Explore q
        | isOk maze q p || b && S.member p s -> commit
        | not b -> (s, Fail)
    _ -> (s, st)
  where
    commit = (S.insert p s, if p == fst (bounds maze) then Success else Explore p)

------------------------ GUI

mazeGame :: Widget
mazeGame = do
    forgiving <- newRef False
    let init = (0,(4,4))
    dim_ <- newRef init
    let dim = _2 `lensMap` dim_
        mazekind = _1 `lensMap` dim_
        dimX = (_2 . iso id (max 1 . min 40)) `lensMap` toEqRef dim
        dimY = (_1 . iso id (max 1 . min 40)) `lensMap` toEqRef dim
        genMaze (0, d) = Maze1.genMaze d
        genMaze (1, d) = Maze2.genMaze d
    maze_ <- extRef_ dim_ (runState (genMaze init) (mkStdGen 323401)) $ \d (_, s) -> runState (genMaze d) s
    r <- extRef_ maze_ (S.empty, Start) $ \_ _ -> (S.empty, Start)

    let handler (MoveTo (MousePos _ [p]), _) = domove p
        handler _ = pure ()

        domove p = do
            (maze, _) <- readerToWriter $ readRef maze_
            b <- readerToWriter $ readRef forgiving
            modRef r $ gameLogic b maze p

        move f = do
            (maze, _) <- readerToWriter $ readRef maze_
            (_, st) <- readerToWriter $ readRef r
            let m = case st of
                    Start -> Just $ snd $ bounds maze
                    Explore p -> checkBounds (bounds maze) $ f p
                    _ -> Nothing
            maybe (pure True) ((>> pure True) . domove) m

        key (SimpleKey Key'Left)  = move $ \(x,y)->(x-1,y)
        key (SimpleKey Key'Right) = move $ \(x,y)->(x+1,y)
        key (SimpleKey Key'Up)    = move $ \(x,y)->(x,y+1)
        key (SimpleKey Key'Down)  = move $ \(x,y)->(x,y-1)
        key _ = pure False

        pos maze Start = Just $ snd $ bounds maze
        pos maze Success = Just $ fst $ bounds maze
        pos _ (Explore p) = Just p
        pos _ _ = Nothing

    vertically
        [ horizontally
            [ canvas 400 400 1 handler (Just key) (liftA2 (\(m,_) (s, st) -> (m,s, pos m st)) (readRef maze_) (readRef r)) drawMaze

            , vertically
                [ horizontally
                    [ checkbox forgiving
                    , label $ pure "forgiving mode"
                    ]
                , combobox ["cdsmith's", "Mihai Maruseac's"] mazekind
                , label $ pure "maze generator"
                ]
            ]

        , label $ fmap (show . snd) $ readRef r
        , horizontally
            [ button (pure "Try again") $ pure $ Just $ modRef maze_ id
            , button (pure "New maze") $ pure $ Just $ modRef dim id
            ]
        , horizontally
            [ entryShow dimX
            , smartButton (pure "+1") dimX succ
            , smartButton (pure "-1") dimX pred
            , label $ pure "width"
            ]
        , horizontally
            [ entryShow dimY
            , smartButton (pure "+1") dimY succ
            , smartButton (pure "-1") dimY pred
            , label $ pure "height"
            ]
        ]

----------------------------- utils

extRef_ ::  Ref b -> a -> (b -> a -> a) -> RefCreator (Ref a)
extRef_ r def f = do
    r0 <- readerToCreator $ readRef r
    v <- extendRef r (lens fst set) (r0, def)
    pure $ _2 `lensMap` v
  where
    set (_, y) x = (x, f x y)
