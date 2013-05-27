-- | An integer list editor
module LGtk.Demos.IntListEditor where

import LGtk

import Control.Monad
import Control.Arrow ((***))
import Data.List
import Data.Function (on)
import Prelude hiding ((.), id)

---------------

intListEditor
    :: EffRef m
    => Ref m [(Int, Bool)]  -- ^ state reference
    -> Ref m Bool           -- ^ settings reference
    -> Widget m
intListEditor list range = action $ do
    (undo, redo)  <- undoTr ((==) `on` map fst) list
    return $ notebook
        [ (,) "Editor" $ vcat
            [ hcat
                [ entryShow $ toRef len
                , smartButton' (return "+1") len (+1)
                , smartButton' (return "-1") len (+(-1))
                , smartButton' (liftM (("DeleteAll " ++) . show) $ readRef $ toRef len) len $ const 0
                , button (return "undo") undo
                , button (return "redo") redo
                ]
            , hcat
                [ smartButton (return "+1")         list $ map $ mapFst (+1)
                , smartButton (return "-1")         list $ map $ mapFst (+(-1))
                , smartButton (return "sort")       list $ sortBy (compare `on` fst)
                , smartButton (return "SelectAll")  list $ map $ mapSnd $ const True
                , smartButton (return "SelectPos")  list $ map $ \(a,_) -> (a, a>0)
                , smartButton (return "SelectEven") list $ map $ \(a,_) -> (a, even a)
                , smartButton (return "InvertSel")  list $ map $ mapSnd not
                , smartButton (liftM (("DelSel " ++) . show . length) sel) list $ filter $ not . snd
                , smartButton' (return "CopySel") safeList $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]
                , smartButton (return "+1 Sel")     list $ map $ mapSel (+1)
                , smartButton (return "-1 Sel")     list $ map $ mapSel (+(-1))
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

    safeList = EqRef list $ return $ lens id (const . take maxi)

    sel = liftM (filter snd) $ readRef list

    len = EqRef (toRef safeList) $ liftM (lens length . extendList) $ readRef range
    extendList r n xs = take n $ (reverse . drop 1 . reverse) xs ++
        (uncurry zip . ((if r then enumFrom else repeat) *** repeat)) (head $ reverse xs ++ [def])

    def = (0, True)
    maxi = 15

    mapFst f (x, y) = (f x, y)
    mapSnd f (x, y) = (x, f y)
    mapSel f (x, y) = (if y then f x else x, y)

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



