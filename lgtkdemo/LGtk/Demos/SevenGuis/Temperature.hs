-- | Task 2 of https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module LGtk.Demos.SevenGuis.Temperature ( temperatureConverter ) where

import Control.Lens
import Control.Lens.Extras (is)
import LGtk
import Numeric.Lens

temperatureConverter :: Widget
temperatureConverter = do
    x <- newRef 0
    hcat [ entryShow x
         , label (return "Celsius = ")
         , entryShow (celsiusToFahrenheit `lensMap` x)
         , label (return "Fahrenheit")
         ]

celsiusToFahrenheit :: Iso' Double Double
celsiusToFahrenheit = multiplying 1.8 . adding 32
