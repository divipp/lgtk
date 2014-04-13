-- | An integer list editor
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LGtk.Demos.IntListEditor where

import Control.Monad
import Data.List (sortBy)
import Data.Function (on)

import Control.Lens
import LGtk

intListEditor
    :: forall m a
    .  (EffRef m, Read a, Show a, Integral a)
    => (a, Bool)            -- ^ default element
    -> Int                  -- ^ maximum number of elements
    -> Ref m [(a, Bool)]    -- ^ state reference
    -> Ref m Bool           -- ^ settings reference
    -> Widget m
intListEditor def maxi list_ range = do
    (undo, redo)  <- undoTr ((==) `on` map fst) list_
    notebook
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
                [ smartButton (return "+1")         list $ map $ over _1 (+1)
                , smartButton (return "-1")         list $ map $ over _1 (+(-1))
                , smartButton (return "sort")       list $ sortBy (compare `on` fst)
                , smartButton (return "SelectAll")  list $ map $ set _2 True
                , smartButton (return "SelectPos")  list $ map $ \(a,_) -> (a, a>0)
                , smartButton (return "SelectEven") list $ map $ \(a,_) -> (a, even a)
                , smartButton (return "InvertSel")  list $ map $ over _2 not
                , smartButton (liftM (("DelSel " ++) . show . length) sel) list $ filter $ not . snd
                , smartButton (return "CopySel") safeList $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]
                , smartButton (return "+1 Sel")     list $ map $ mapSel (+1)
                , smartButton (return "-1 Sel")     list $ map $ mapSel (+(-1))
                ]
            , label $ liftM (("Sum: " ++) . show . sum . map fst) sel
            , listEditor def (map itemEditor [0..]) list_
            ]
        , (,) "Settings" $ hcat
            [ label $ return "Create range"
            , checkbox range
            ]
        ]
 where
    list = eqRef list_

    itemEditor i r = hcat
        [ label $ return $ show (i+1) ++ "."
        , entryShow $ _1 `lensMap` r
        , checkbox $ _2 `lensMap` r
        , button_ (return "Del")  (return True) $ modRef list $ \xs -> take i xs ++ drop (i+1) xs
        , button_ (return "Copy") (return True) $ modRef list $ \xs -> take (i+1) xs ++ drop i xs
        ]

    safeList = lens id (const $ take maxi) `lensMap` list

    sel = liftM (filter snd) $ readRef list

    len = readRef range >>= \r -> ll r `lensMap` safeList
    ll :: Bool -> Lens' [(a, Bool)] Int
    ll r = lens length extendList where
        extendList xs n = take n $ (reverse . drop 1 . reverse) xs ++
            (uncurry zip . (iterate (+ if r then 1 else 0) *** repeat)) (head $ reverse xs ++ [def])

    mapSel f (x, y) = (if y then f x else x, y)

    (f *** g) (a, b) = (f a, g b)

listEditor :: EffRef m => a -> [Ref m a -> Widget m] -> Ref m [a] -> Widget m
listEditor def (ed: eds) r = do
    q <- extRef r listLens (False, (def, []))
    cell (liftM fst $ readRef q) $ \b -> case b of
        False -> empty
        True -> vcat 
            [ ed $ _2 . _1 `lensMap` q
            , listEditor def eds $ _2 . _2 `lensMap` q
            ]



