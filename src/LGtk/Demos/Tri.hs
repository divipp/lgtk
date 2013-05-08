{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{- |
An editor for integers x, y, z such that x + y = z always hold and
the last edited value change.
-}
module LGtk.Demos.Tri where

import LGtk

import Control.Monad
import Prelude hiding ((.), id)

-- | Information pieces: what is known?
data S = X Int | Y Int | XY Int

-- | Getter
getX, getY, getXY :: [S] -> Int
getX s =  head $ [x | X  x <- s]  ++ [getXY s - getY s]
getY s =  head $ [x | Y  x <- s]  ++ [getXY s - getX s]
getXY s = head $ [x | XY x <- s]  ++ [getX  s + getY s]

-- | Setter
setX, setY, setXY :: Int -> [S] -> [S]
setX  x s = take 2 $ X  x : filter (\x-> case x of X  _ -> False; _ -> True) s
setY  x s = take 2 $ Y  x : filter (\x-> case x of Y  _ -> False; _ -> True) s
setXY x s = take 2 $ XY x : filter (\x-> case x of XY _ -> False; _ -> True) s

-- | The editor
tri :: EffRef m => I m
tri = Action $ do
    s <- newRef [X 0, Y 0]
    return $ vcat
        [ hcat [entry $ showLens . lens getX setX % s, Label $ constSend "x"]
        , hcat [entry $ showLens . lens getY setY % s, Label $ constSend "y"]
        , hcat [entry $ showLens . lens getXY setXY % s, Label $ constSend "x + y"]
        ]






