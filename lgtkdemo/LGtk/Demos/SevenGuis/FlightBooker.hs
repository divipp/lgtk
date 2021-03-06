-- | https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module LGtk.Demos.SevenGuis.FlightBooker (flightBooker) where

import Control.Applicative ( (<$>) )
import Control.Monad
import Control.Lens
import Control.Lens.Extras (is)
import LGtk
import Numeric
import Numeric.Lens
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format

newtype Date = Date LocalTime
{-
timeLocale = defaultTimeLocale

instance Show Date where
  show (Date d) = formatTime timeLocale d

instance Read Date where
  read str = Date <$>
-}
flightBooker :: Widget
flightBooker = emptyWidget {-do
    mode <- newRef 0
    start <- newRef ""
    end <- newRef ""
    vertically
        [ combobox ["one-way flight", "return flight"] mode
        , entryShow start
        , entryShow end
        , button (return "Book") $ return $ Nothing
        ]-}
