-- | Task 1 of https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module LGtk.Demos.SevenGuis.Counter ( counter ) where

import LGtk

counter :: Widget
counter = do
    x <- newEqRef (0 :: Int)
    hcat [entryShow x, smartButton (return "Count") x (+1)]
