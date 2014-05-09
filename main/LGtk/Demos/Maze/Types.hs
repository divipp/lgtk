module LGtk.Demos.Maze.Types
where

import Data.Array (Array)
import Data.Vector (Vector)

{-
Common types are presented in this file.
-}

{- Small typedefs -}
type Length = Int
type Coord = Int
type Size = (Length, Length)
type Point = Size
type Dir = Size
type Time = Int
type Fitness = Int

{- The cardinal directions. -}
data Cardinal = N | E | S | W deriving (Eq, Show, Read, Ord, Enum)

{- A cell. The list contains the openings. -}
newtype Cell = C [Cardinal] deriving (Eq, Show, Read)

{- Simple type for maze. -}
type Maze = Array Size Cell

{- A plan is a vector of directions to go, at each time step. -}
type Plan = Vector Cardinal

