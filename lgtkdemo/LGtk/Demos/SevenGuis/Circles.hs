{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | Task 6 of https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module LGtk.Demos.SevenGuis.Circles ( circleDrawer ) where

import           Control.Lens hiding ((#))
import qualified Data.IntMap as IM
import           Data.List (sortBy)
import           Data.Maybe
import           Data.Ord (comparing)
import           Diagrams.Prelude hiding (at)
import           LGtk hiding (value)
import           Prelude
import           Safe (headMay)

data Circle = Circle
    { circlePos :: P2
    , circleRadius :: Double
    } deriving (Eq)

radiusLens :: Lens' Circle Double
radiusLens f (Circle p r) = Circle p <$> f r

drawCircle :: Maybe Int -> Int -> Circle -> Dia [Int]
drawCircle cur i Circle {..} =
    moveTo circlePos (circle circleRadius)
    # stroke
    # lc black
    # lw (Output 1)
    # (if cur == Just i
          then fc gray
          else id)
    # value [i]

circleDrawer :: Widget
circleDrawer = do
    state <- newRef $ IM.fromList $ zip [0..]
        [ Circle (25     ^& 60) 40
        , Circle ((-120) ^& 60) 20
        , Circle (70     ^& 80) 10
        ]
    focused <- newRef Nothing
    (undo, redo, pushState) <- undoPush (==) state
    let mouse (Click (MousePos pos []), _) = do
            -- Push a new undo entry.  Often this will do nothing, but
            -- if the user was in the middle of resizing a circle,
            -- this will give that change its own entry in the
            -- history.
            pushState
            -- Insert a new circle at the location the user clicked.
            modRef state $ \circles -> do
                IM.insert (nextKey circles) (Circle pos 10) circles
            -- Record the insertion of the circle as a new undo entry.
            pushState
        mouse (Click (MousePos pos xs), _) = do
            -- Similarly to the first pushState above, this use of
            -- pushState handles the case where the user has clicked
            -- away from resizing a circle.
            pushState
            -- Focus the circle with the center nearest to the mouse
            -- click.
            circles <- readerToWriter $ readRef state
            writeRef focused
                $ fmap fst
                $ headMay
                $ sortBy (comparing $ distance pos . circlePos . snd)
                $ mapMaybe (\x -> (x,) <$> IM.lookup x circles) xs
        mouse _ = pure ()
        clearFocusWhenPressed =
            fmap (fmap (writeRef focused Nothing >>))
    vertically
        [ horizontally [ button (pure "Undo") (clearFocusWhenPressed undo)
                       , button (pure "Redo") (clearFocusWhenPressed redo)
                       ]
        , canvas 400 400 400 mouse Nothing
            -- Collect the state which determines the rendered image.
            ((,) <$> readRef focused <*> readRef state)
            -- Draw the image by overlapping all of the circles
            -- ('ifoldMap' uses (<>) to merge the items together,
            -- which overlaps the 'Diagram's).
            (\(cur, xs) -> ifoldMap (drawCircle cur) xs)
        -- The radius adjustment slider is only shown if a circle is
        -- currently focused.
        , cell (readRef focused) $ \case
            Nothing -> emptyWidget
            Just cur -> do
                -- This is dangerous!  For example, if
                -- 'clearFocusWhenPressed' isn't used above, then undo
                -- / redo can cause the currently focused circle to no
                -- longer exist.  It'd be nice to have something
                -- better here.
                let curRadiusLens :: Lens' (IM.IntMap Circle) Double
                    curRadiusLens = at cur . iso fromJust Just . radiusLens
                horizontally [ hscale 1 100 1 (curRadiusLens `lensMap` state)
                             , button (pure "Apply") $ pure $ Just $ do
                                 writeRef focused Nothing
                                 pushState
                             ]
        ]

nextKey :: IM.IntMap a -> Int
nextKey = maybe 0 ((+1) . fst . fst) . IM.maxViewWithKey
