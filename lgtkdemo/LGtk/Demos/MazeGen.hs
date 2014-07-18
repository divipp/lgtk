module LGtk.Demos.MazeGen
    ( genMaze
    ) where

import Data.List
import Data.Array.IArray
import System.Random
import Data.Equivalence.Persistent
import System.Random.Shuffle

import Control.Monad.State
import LGtk.Demos.Maze.Types hiding (Cell)

------------ copied from http://cdsmith.wordpress.com/2011/06/06/mazes-in-haskell-my-version/ on 9 May, 2014

-- Vertical walls are to the right of their cell (so the x component
-- must be less than width - 1), and horizontal walls are to the top
-- of their cell (so the y component must be less than height - 1).

type Cell = (Int, Int)
data Wall = H Cell | V Cell deriving (Eq, Show)

process _     []                = []
process rooms (H (x,y) : ws)
    | equiv rooms (x,y) (x,y+1) = H (x,y) : process rooms ws
    | otherwise                 = process (equate (x,y) (x,y+1) rooms) ws
process rooms (V (x,y) : ws)
    | equiv rooms (x,y) (x+1,y) = V (x,y) : process rooms ws
    | otherwise                 = process (equate (x,y) (x+1,y) rooms) ws

genMaze_ :: RandomGen gen => Int -> Int -> gen -> [Wall]
genMaze_ w h gen = finalWalls
  where allWalls = [ H (x,y) | x <- [0 .. w-1], y <- [0 .. h-2] ]
                ++ [ V (x,y) | x <- [0 .. w-2], y <- [0 .. h-1] ]
        startRooms = emptyEquivalence ((0,0), (w-1, h-1))
        startWalls = shuffle' allWalls (length allWalls) gen
        finalWalls = process startRooms startWalls

------------ end of copy

genMaze :: Size -> State StdGen Maze
genMaze (w, h) = state f where
    f s = (tr $ genMaze_ w h s1, s2)
      where
        (s1, s2) = split s

    tr ls = array ((1,1), (w,h)) [ ((i,j), C $ complement $ concatMap (g i j) ls) | i <- [1..w], j<-[1..h]]
      where
        g i j (V (x,y)) | i == x + 1 && j == y + 1 = [S]
        g i j (V (x,y)) | i == x + 2 && j == y + 1 = [N]
        g i j (H (x,y)) | i == x + 1 && j == y + 1 = [E]
        g i j (H (x,y)) | i == x + 1 && j == y + 2 = [W]
        g _ _ _ = []


complement :: [Cardinal] -> [Cardinal]
complement = f [N,E,S,W] . sort
  where
    f (x:xs) (y:ys) | x == y = f xs ys
    f (x:xs) ys = x: f xs ys
    f [] _ = []
