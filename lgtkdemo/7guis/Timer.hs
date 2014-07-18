-- | https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module Timer where

import Control.Monad
import Control.Lens
import Control.Lens.Extras (is)
import Data.LensRef
import LGtk
import Numeric
import Numeric.Lens

main :: IO ()
main = runWidget $ do
    d <- newRef 10.0
    e <- liftM (lensMap _2) $ extRef d (lens fst $ \(_, t) d -> (d, min t d) ) (0, 0)
    let ratio = liftM2 (/) (readRef e) (readRef d)
    onChange ratio $ const $ do
      t <- readRef e
      duration <- readRef d
      when (t < duration) $ asyncWrite 20000 $ writeRef e $ min duration $ t + 0.02
    vcat [ hcat [ label (return "Elapsed Time: ")
                , progress ratio
                ]
         , hcat [ vcat [ label $ liftM (\v -> showFFloat (Just 2) v $ "s left") $ readRef e
                       , label $ return "Duration:  " ]
                , hscale 0.0 60.0 10.0 d
                ]
         , button (return "Reset") $ return $ Just $ writeRef e 0
         ]