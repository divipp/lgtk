{-# LANGUAGE RecordWildCards #-}

module Circles where

import           Control.Lens hiding ((#))
import           Data.Colour.Names (gray)
import           Data.Foldable
import qualified Data.IntMap as IM
import           Diagrams.Prelude hiding (hcat, vcat)
import           LGtk as G

data Circle = Circle
    { circlePos :: R2
    , circleRadius :: Double
    , circleFocused :: Bool
    } deriving (Eq)

main = runWidget $ do
    let undo = return ()
        redo = return ()
        -- mouse (Click _, ix) =
        mouse _ = return ()
    state <- newRef $ IM.fromList $
        [ (0, Circle (50 ^& 60) 10 True)
        ]
    vcat
        [ hcat [button (return "Undo") (return (Just undo)), button (return "Redo") (return (Just redo))]
        , canvas 480 480 480 mouse Nothing (readRef state) (ifoldMap drawCircle)
        ]

drawCircle :: Int -> Circle -> Dia [Int]
drawCircle ix Circle {..} =
    translate circlePos (circle circleRadius)
    # stroke
    # lc black
    # lw 1
    # (if circleFocused
          then fc gray
          else id)
    # value [ix]
