-- | https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

{-# LANGUAGE ScopedTypeVariables #-}
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

{-
data WidgetCore m
    = Label (RefReader m String)     -- ^ label
    | Button { label_  :: RefReader m String
             , sensitive_ :: RefReader m Bool
             , color_ :: Maybe (RefReader m (Colour Double))
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (String -> Bool) (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (RefReader m b) (forall x . (Widget m -> m x) -> b -> m (m x))
    | forall a b . (Eq b, Monoid a, Semigroup a)
    => Canvas
        Int     -- width
        Int     -- height
        Double  -- scale
        ((MouseEvent a, Dia a) -> Modifier m ())    -- mouse event handler
        (KeyboardHandler (Modifier m))              -- keyboard event handler
        (RefReader m b)
        (b -> Dia a)
    | Scale Double Double Double (SendReceive m Double)
-}
