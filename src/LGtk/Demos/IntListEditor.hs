{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | An integer list editor
module LGtk.Demos.IntListEditor where

import LGtk

import Control.Monad
import qualified Control.Arrow as Arrow
import Data.List
import Data.Function (on)
import Prelude hiding ((.), id)

---------------

intListEditor
    :: EffRef m
    => Ref m String         -- ^ state reference
    -> Ref m String         -- ^ settings reference
    -> Widget m
intListEditor state settings = action $ do
    list <- extRef state showLens []
    (undo, redo)  <- undoTr ((==) `on` map fst) list
    range <- extRef settings showLens True
    let safe = lens id (const . take maxi)
        len = liftM (\r -> lens length $ extendList r . min maxi) $ readRef range
        sel = liftM (filter snd) $ readRef list
    return $ notebook
        [ (,) "Editor" $ vcat
            [ hcat
                [ entryShow $ joinRef $ liftM (`lensMap` list) len
                , smartButton (return "+1") list $ modL' len (+1)
                , smartButton (return "-1") list $ modL' len (+(-1))
                , smartButton (liftM (("DeleteAll " ++) . show) $ len >>= \k -> readRef $ k `lensMap` list) list $ modL' len $ const 0
                , button (return "undo") undo
                , button (return "redo") redo
                ]
            , hcat
                [ sbutton (return "+1")         list $ map $ mapFst (+1)
                , sbutton (return "-1")         list $ map $ mapFst (+(-1))
                , sbutton (return "sort")       list $ sortBy (compare `on` fst)
                , sbutton (return "SelectAll")  list $ map $ mapSnd $ const True
                , sbutton (return "SelectPos")  list $ map $ \(a,_) -> (a, a>0)
                , sbutton (return "SelectEven") list $ map $ \(a,_) -> (a, even a)
                , sbutton (return "InvertSel")  list $ map $ mapSnd not
                , sbutton (liftM (("DelSel " ++) . show . length) sel) list $ filter $ not . snd
                , smartButton (return "CopySel") list $ modL_ safe $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]
                , sbutton (return "+1 Sel")     list $ map $ mapSel (+1)
                , sbutton (return "-1 Sel")     list $ map $ mapSel (+(-1))
                ]
            , label $ liftM (("Sum: " ++) . show . sum . map fst) sel
            , action $ listEditor def (itemEditor list) list
            ]
        , (,) "Settings" $ hcat
            [ label $ return "Create range"
            , checkbox range
            ]
        ]
 where
    itemEditor list i r = return $ hcat
        [ label $ return $ show (i+1) ++ "."
        , entryShow $ fstLens `lensMap` r
        , checkbox $ sndLens `lensMap` r
        , button_ (return "Del")  (return True) $ modRef list $ \xs -> take i xs ++ drop (i+1) xs
        , button_ (return "Copy") (return True) $ modRef list $ \xs -> take (i+1) xs ++ drop i xs
        ]

    modL' mr f b = do
        r <- mr
        modL_ r f b

    modL_ r f b = return $ modL r f b

    extendList r n xs = take n $ (reverse . drop 1 . reverse) xs ++
        (uncurry zip . ((if r then enumFrom else repeat) Arrow.*** repeat)) (head $ reverse xs ++ [def])

    def = (0, True)
    maxi = 15

    mapFst f (x, y) = (f x, y)
    mapSnd f (x, y) = (x, f y)
    mapSel f (x, y) = (if y then f x else x, y)

    sbutton s k f = smartButton s k $ return . f


listEditor :: EffRef m => a -> (Int -> Ref m a -> m (Widget m)) -> Ref m [a] -> m (Widget m)
listEditor def ed = editor 0 where
  editor i r = do
    q <- extRef r listLens (False, (def, []))
    return $ cell (liftM fst $ readRef q) $ \b -> case b of
        False -> empty
        True -> action $ do
            t1 <- ed i $ fstLens . sndLens `lensMap` q
            t2 <- editor (i+1) $ sndLens . sndLens `lensMap` q
            return $ vcat [t1, t2]



