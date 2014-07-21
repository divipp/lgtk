-- | https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module LGtk.Demos.SevenGuis.Timer (timer) where

import Control.Monad
import Control.Lens
import LGtk
import Numeric

timer :: Widget
timer = do
    d <- newRef 10.0
    e <- liftM (lensMap _2) $ extendRef d (lens fst $ \(_, t) d -> (d, min t d) ) (0, 0)
    let ratio = liftM2 (/) (readRef e) (readRef d) <&> min 1 . max 0
    _ <- onChange ratio $ const $ do
        t <- readerToCreator $ readRef e
        duration <- readerToCreator $ readRef d
        when (t < duration) $ asyncWrite 20000 $ writeRef e $ min duration $ t + 0.02
    vertically
        [ horizontally
            [ label (return "Elapsed Time: ")
            , progress ratio
            ]
        , horizontally
            [ vertically
                [ label $ liftM (\v -> showFFloat (Just 2) v $ "s") $ readRef e
                , label $ return "Duration:  "
                ]
            , hscale 0.0 60.0 10.0 d
            ]
        , button (return "Reset") $ return $ Just $ writeRef e 0
        ]
