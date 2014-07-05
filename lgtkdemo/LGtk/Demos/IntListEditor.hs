-- | An integer list editor
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LGtk.Demos.IntListEditor where

import Control.Applicative
import Data.List (sortBy)
import Data.Function (on)
import Control.Lens

import LGtk

intListEditor
    :: forall a
    .  (Read a, Show a, Integral a)
    => (a, Bool)            -- ^ default element
    -> Int                  -- ^ maximum number of elements
    -> Ref [(a, Bool)]    -- ^ state reference
    -> Ref Bool           -- ^ settings reference
    -> Widget
intListEditor def maxi list_ range = do
    (undo, redo)  <- undoTr ((==) `on` map fst) list_
    notebook
        [ (,) "Editor" $ vertically
            [ horizontally
                [ entryShow len
                , vertically
                    [ horizontally
                        [ smartButton (pure "+1") len (+1)
                        , smartButton (pure "-1") len (+(-1))
                        , smartButton (fmap (("DeleteAll " ++) . show) $ readRef len) len $ const 0
                        ]
                    , horizontally
                        [ button (pure "undo") undo
                        , button (pure "redo") redo
                        ]
                    ]
                ]
            , horizontally
                [ smartButton (pure "+1")         list $ map $ over _1 (+1)
                , smartButton (pure "-1")         list $ map $ over _1 (+(-1))
                , smartButton (pure "sort")       list $ sortBy (compare `on` fst)
                ]
            , horizontally
                [ smartButton (pure "SelectAll")  list $ map $ set _2 True
                , smartButton (pure "SelectPos")  list $ map $ \(a,_) -> (a, a>0)
                , smartButton (pure "SelectEven") list $ map $ \(a,_) -> (a, even a)
                , smartButton (pure "InvertSel")  list $ map $ over _2 not
                ]
            , horizontally
                [ smartButton (fmap (("DelSel " ++) . show . length) sel) list $ filter $ not . snd
                , smartButton (pure "CopySel") safeList $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]
                , smartButton (pure "+1 Sel")     list $ map $ mapSel (+1)
                , smartButton (pure "-1 Sel")     list $ map $ mapSel (+(-1))
                ]
            , label $ fmap (("Sum: " ++) . show . sum . map fst) sel
            , listEditor def (map itemEditor [0..]) list_
            ]
        , (,) "Settings" $ horizontally
            [ label $ pure "RefCreator range"
            , checkbox range
            ]
        ]
 where
    list = toEqRef list_

    itemEditor i r = horizontally
        [ label $ pure $ show (i+1) ++ "."
        , entryShow $ _1 `lensMap` r
        , checkbox $ _2 `lensMap` r
        , primButton (pure "Del")  (pure True) Nothing $ modRef list $ \xs -> take i xs ++ drop (i+1) xs
        , primButton (pure "Copy") (pure True) Nothing $ modRef list $ \xs -> take (i+1) xs ++ drop i xs
        ]

    safeList = lens id (const $ take maxi) `lensMap` list

    sel = fmap (filter snd) $ readRef list

    len = joinRef $ readRef range >>= \r -> pure $ ll r `lensMap` safeList   -- todo
    ll :: Bool -> Lens' [(a, Bool)] Int
    ll r = lens length extendList where
        extendList xs n = take n $ (reverse . drop 1 . reverse) xs ++
            (uncurry zip . (iterate (+ if r then 1 else 0) *** repeat)) (head $ reverse xs ++ [def])

    mapSel f (x, y) = (if y then f x else x, y)

    (f *** g) (a, b) = (f a, g b)

listEditor ::  a -> [Ref a -> Widget] -> Ref [a] -> Widget
listEditor def (ed: eds) r = do
    q <- extendRef r listLens (False, (def, []))
    cell (fmap fst $ readRef q) $ \b -> case b of
        False -> emptyWidget
        True -> vertically
            [ ed $ _2 . _1 `lensMap` q
            , listEditor def eds $ _2 . _2 `lensMap` q
            ]
