-- | An integer list editor
module LGtk.Demos.IntListEditor where

import LGtk

import Data.List (sortBy)
import Data.Function (on)
import Prelude hiding ((.), id)

intListEditor
    :: (EffRef m, Read a, Show a, Integral a)
    => (a, Bool)            -- ^ default element
    -> Int                  -- ^ maximum number of elements
    -> Ref m [(a, Bool)]    -- ^ state reference
    -> Ref m Bool           -- ^ settings reference
    -> Widget m
intListEditor def maxi list_ range = action $ do
    (undo, redo)  <- undoTr ((==) `on` map fst) list_
    return $ notebook
        [ (,) "Editor" $ vcat
            [ hcat
                [ entryShow len
                , smartButton (return "+1") len (+1)
                , smartButton (return "-1") len (+(-1))
                , smartButton (liftM (("DeleteAll " ++) . show) $ readRef len) len $ const 0
                , button (return "undo") undo
                , button (return "redo") redo
                ]
            , hcat
                [ smartButton (return "+1")         list $ map $ first (+1)
                , smartButton (return "-1")         list $ map $ first (+(-1))
                , smartButton (return "sort")       list $ sortBy (compare `on` fst)
                , smartButton (return "SelectAll")  list $ map $ second $ const True
                , smartButton (return "SelectPos")  list $ map $ \(a,_) -> (a, a>0)
                , smartButton (return "SelectEven") list $ map $ \(a,_) -> (a, even a)
                , smartButton (return "InvertSel")  list $ map $ second not
                , smartButton (liftM (("DelSel " ++) . show . length) sel) list $ filter $ not . snd
                , smartButton (return "CopySel") safeList $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]
                , smartButton (return "+1 Sel")     list $ map $ mapSel (+1)
                , smartButton (return "-1 Sel")     list $ map $ mapSel (+(-1))
                ]
            , label $ liftM (("Sum: " ++) . show . sum . map fst) sel
            , action $ listEditor def (map itemEditor [0..]) list_
            ]
        , (,) "Settings" $ hcat
            [ label $ return "Create range"
            , checkbox range
            ]
        ]
 where
    list = eqRef list_

    itemEditor i r = return $ hcat
        [ label $ return $ show (i+1) ++ "."
        , entryShow $ fstLens `lensMap` r
        , checkbox $ sndLens `lensMap` r
        , button_ (return "Del")  (return True) $ modRef list $ \xs -> take i xs ++ drop (i+1) xs
        , button_ (return "Copy") (return True) $ modRef list $ \xs -> take (i+1) xs ++ drop i xs
        ]

    safeList = lens id (const . take maxi) `lensMap` list

    sel = liftM (filter snd) $ readRef list

    len = joinRef $ liftM ((`lensMap` safeList) . lens length . extendList) $ readRef range
    extendList r n xs = take n $ (reverse . drop 1 . reverse) xs ++
        (uncurry zip . (iterate (+ if r then 1 else 0) *** repeat)) (head $ reverse xs ++ [def])

    mapSel f (x, y) = (if y then f x else x, y)

listEditor :: EffRef m => a -> [Ref m a -> m (Widget m)] -> Ref m [a] -> m (Widget m)
listEditor def (ed: eds) r = do
    q <- extRef r listLens (False, (def, []))
    return $ cell (liftM fst $ readRef q) $ \b -> case b of
        False -> empty
        True -> action $ do
            t1 <- ed $ fstLens . sndLens `lensMap` q
            t2 <- listEditor def eds $ sndLens . sndLens `lensMap` q
            return $ vcat [t1, t2]



