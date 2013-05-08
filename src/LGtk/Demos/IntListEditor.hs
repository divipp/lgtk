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
    -> I m
intListEditor state settings = Action $ do
    list <- extRef state showLens []
    (undo, redo)  <- undoTr ((==) `on` map fst) list
    range <- extRef settings showLens True
    let safe = lens id (const . take maxi)
        len = liftM (\r -> lens length $ extendList r . min maxi) $ readRef range
        sel = liftM (filter snd) $ readRef list
    return $ notebook
        [ (,) "Editor" $ vcat
            [ hcat
                [ entry $ joinRef $ liftM (\k -> showLens . k % list) len
                , smartButton (constEffect "+1") (modL' len (+1))      list
                , smartButton (constEffect "-1") (modL' len (+(-1)))   list
                , smartButton (rEffect $ liftM (("DeleteAll " ++) . show) $ len >>= \k -> readRef $ k % list) (modL' len $ const 0) list
                , button (constEffect "undo") undo
                , button (constEffect "redo") redo
                ]
            , hcat
                [ sbutton (constEffect "+1")         (map $ mapFst (+1))           list
                , sbutton (constEffect "-1")         (map $ mapFst (+(-1)))        list
                , sbutton (constEffect "sort")       (sortBy (compare `on` fst))   list
                , sbutton (constEffect "SelectAll")  (map $ mapSnd $ const True)   list
                , sbutton (constEffect "SelectPos")  (map $ \(a,_) -> (a, a>0))    list
                , sbutton (constEffect "SelectEven") (map $ \(a,_) -> (a, even a)) list
                , sbutton (constEffect "InvertSel")  (map $ mapSnd not)            list
                , sbutton (rEffect $ liftM (("DelSel " ++) . show . length) sel) (filter $ not . snd) list
                , smartButton (constEffect "CopySel") (modL_ safe $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]) list
                , sbutton (constEffect "+1 Sel")     (map $ mapSel (+1))           list
                , sbutton (constEffect "-1 Sel")     (map $ mapSel (+(-1)))        list
                ]
            , Label $ rEffect $ liftM (("Sum: " ++) . show . sum . map fst) sel
            , Action $ listEditor def (itemEditor list) list
            ]
        , (,) "Settings" $ hcat
            [ Label $ constEffect "Create range"
            , checkbox range
            ]
        ]
 where
    itemEditor list i r = return $ hcat
        [ Label $ constEffect $ show (i+1) ++ "."
        , entry $ showLens . fstLens % r
        , checkbox $ sndLens % r
        , Button (constEffect "Del")  voidReceiver $ addWEffect $ const $ modRef list $ \xs -> take i xs ++ drop (i+1) xs
        , Button (constEffect "Copy") voidReceiver $ addWEffect $ const $ modRef list $ \xs -> take (i+1) xs ++ drop i xs
        ]

    modL' mr f b = do
        r <- mr
        modL_ r f b

    modL_ r f b = return $ modL r f b

    extendList r n xs = take n $ (reverse . drop 1 . reverse) xs ++
        (uncurry zip . ((if r then enumFrom else repeat) Arrow.*** repeat)) (head $ reverse xs ++ [def])

    def = (0, True)
    maxi = 15

    sbutton s f k = smartButton s (return . f) k

    mapFst f (x, y) = (f x, y)
    mapSnd f (x, y) = (x, f y)
    mapSel f (x, y) = (if y then f x else x, y)


listEditor :: (MonadRegister m, ExtRef m, Inner m ~ Inner' m) => a -> (Int -> Ref m a -> C m (I m)) -> Ref m [a] -> C m (I m)
listEditor def ed = editor 0 where
  editor i r = liftM Action $ memoRead $ do
    q <- extRef r listLens (False, (def, []))
    t1 <- ed i $ fstLens . sndLens % q
    t2 <- editor (i+1) $ sndLens . sndLens % q
    return $ Cell' $ \g -> addICEffect True $ IC (liftM fst $ readRef q) $ \b -> g $ vcat $ if b then [t1, t2] else []



