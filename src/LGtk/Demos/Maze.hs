{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LGtk.Demos.Maze where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Array
import qualified Data.Set as S
import System.Random
import Diagrams.Prelude hiding (vcat, hcat, Point, Start)

import Control.Lens hiding ((#))
import LGtk

import LGtk.Demos.Maze.Types
import LGtk.Demos.Maze.Maze

---------------------------------- Game state

data GameState
    = Start
    | Explore Point
    | Fail
    | Success

instance Show GameState where
    show Success = "Congratulation!"
    show Fail = "Failure, try again!"
    show Start = "Move the pointer to the green circle"
    show (Explore _) = "Reach the blue circle"

------------------------ maze drawing

drawMaze :: (Maze, S.Set Point) -> Dia [Point]
drawMaze (maze, hi) =
    (  mconcat (map drawCell $ assocs maze) # centerXY
    <> rect (fromIntegral $ x2-x1+1) (fromIntegral $ y2-y1+1) # lw 0.005 # fc (sRGB 0.95 0.95 0.95) # value []
    ) # scale (1 / fromIntegral (max (x2-x1+1) (y2-y1+1)))
  where
    drawCell (p@(i,j), C cs) =
            (   (if b then mconcat (map drawWall $ complement cs) # lw 0.005 # value [] else mempty)
            <>  (if p == q2 then circle 0.35 # lw 0.003 # fc green # value [] else mempty)
            <>  (if p == q1 then circle 0.35 # lw 0.003 # fc blue # value [] else mempty)
            <>  rect 1 1 # lw 0 # (if b then fc yellow else id) # value [p]
            )   # translate (r2 (fromIntegral i, fromIntegral j))
        where b = S.member p hi

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

gameLogic b maze p (s, st) = case st of
    Start | p == snd (bounds maze) -> commit
    Explore q
        | isOk maze q p || b && S.member p s -> commit
        | not b -> (s, Fail)
    _ -> (s, st)
  where
    commit = (S.insert p s, if p == fst (bounds maze) then Success else Explore p)

------------------------ GUI

mazeGame :: forall m . EffRef m => Widget m
mazeGame = do
    forgiving <- newRef False
    let init = (4,4)
    dim <- newRef init
    let dimX = (_2 . iso id (max 1 . min 20)) `lensMap` eqRef dim
    let dimY = (_1 . iso id (max 1 . min 20)) `lensMap` eqRef dim
    maze_ <- extRef_ dim (runState (genMaze init) (mkStdGen 323401)) $ \d (_, s) -> runState (genMaze d) s
    r <- extRef_ maze_ (S.empty, Start) $ \_ _ -> (S.empty, Start)

    let handler (MoveTo (MousePos _ [p]), _) = do
            (maze, _) <- readRef' maze_
            b <- readRef' forgiving
            modRef r $ gameLogic b maze p
        handler _ = return ()

    vcat
        [ canvas 400 400 1 handler Nothing (liftM2 (\(m,_) (s,_) -> (m,s)) (readRef maze_) (readRef r)) drawMaze
        , label $ liftM (show . snd) $ readRef r
        , hcat
            [ button (return "Try again") $ return $ Just $ modRef maze_ id
            , button (return "New maze") $ return $ Just $ modRef dim id
            ]
        , hcat
            [ entryShow dimX
            , smartButton (return "+1") dimX succ
            , smartButton (return "-1") dimX pred
            , label $ return "width"
            ]
        , hcat
            [ entryShow dimY
            , smartButton (return "+1") dimY succ
            , smartButton (return "-1") dimY pred
            , label $ return "height"
            ]
        , hcat
            [ checkbox forgiving
            , label $ return "forgiving mode"
            ]
        ]

----------------------------- utils

extRef_ :: EffRef m => Ref m b -> a -> (b -> a -> a) -> m (Ref m a)
extRef_ r def f = do
    r0 <- readRef' r
    v <- extRef r (lens fst set) (r0, def)
    return $ _2 `lensMap` v
  where
    set (_, y) x = (x, f x y)


