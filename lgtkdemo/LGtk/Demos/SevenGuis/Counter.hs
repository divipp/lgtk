-- | Task 1 of https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module LGtk.Demos.SevenGuis.Counter ( counter ) where

import Control.Lens
import LGtk

counter :: Widget
counter = do
    x <- newRef (0 :: Int) <&> toEqRef
    horizontally [entryShow x, smartButton (return "Count") x (+1)]
