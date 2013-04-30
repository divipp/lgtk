{-# LANGUAGE FlexibleContexts #-}
{- |
An editor for integers x, y, z such that x + y = z always hold and
the last edited value change.
-}
module GUI.MLens.Gtk.Demos.Tri where

import GUI.MLens.Gtk

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
tri :: (MonadRegister m, NewRef m, LensReference (Ref m)) => I m
tri = Action $ do
    s <- newRef [X 0, Y 0]
    return $ vcat
        [ hcat [Entry $ showLens . lens getX setX % s, Label $ constEffect "x"]
        , hcat [Entry $ showLens . lens getY setY % s, Label $ constEffect "y"]
        , hcat [Entry $ showLens . lens getXY setXY % s, Label $ constEffect "x + y"]
        ]






