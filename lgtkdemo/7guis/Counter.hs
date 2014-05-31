-- | https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module Counter where

import LGtk

main :: IO ()
main = runWidget $ do
    x <- newEqRef (0 :: Int)
    hcat [entryShow x, smartButton (return "Count") x (+1)]
