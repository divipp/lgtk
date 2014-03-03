{- |
An editor for integers x, y, z such that x + y = z always hold and
the last edited value change.
-}
module LGtk.Demos.Tri where

import Data.Lens.Common
import LGtk

-- | Information pieces: what is known?
data S = X Int | Y Int | XY Int

-- | Getter
getX, getY, getXY :: [S] -> Int
getX s =  head $ [x | X  x <- s]  ++ [getXY s - getY s]
getY s =  head $ [x | Y  x <- s]  ++ [getXY s - getX s]
getXY s = head $ [x | XY x <- s]  ++ [getX  s + getY s]

-- | Setter
setX, setY, setXY :: [S] -> Int -> [S]
setX  s x = take 2 $ X  x : filter (\x-> case x of X  _ -> False; _ -> True) s
setY  s x = take 2 $ Y  x : filter (\x-> case x of Y  _ -> False; _ -> True) s
setXY s x = take 2 $ XY x : filter (\x-> case x of XY _ -> False; _ -> True) s

-- | The editor
tri :: EffRef m => Widget m
tri = action $ do
    s <- newRef [X 0, Y 0]
    return $ vcat
        [ hcat [entryShow $ lens getX  setX  `lensMap` s, label $ return "x"]
        , hcat [entryShow $ lens getY  setY  `lensMap` s, label $ return "y"]
        , hcat [entryShow $ lens getXY setXY `lensMap` s, label $ return "x + y"]
        ]






