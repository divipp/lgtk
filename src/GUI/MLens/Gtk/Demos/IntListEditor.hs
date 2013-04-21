-- | An integer list editor
module GUI.MLens.Gtk.Demos.IntListEditor where

import GUI.MLens.Gtk

import Control.Monad
import qualified Control.Arrow as Arrow
import Data.List
import Data.Function (on)
import Prelude hiding ((.), id)

---------------

intListEditor
    :: (Functor m, ExtRef m)
    => Ref m String         -- ^ state reference
    -> Ref m String         -- ^ settings reference
    -> I m
intListEditor state settings = Action $ do
    list <- extRef state showLens []
    (undo, redo)  <- undoTr ((==) `on` map fst) list
    range <- extRef settings showLens True
    let safe = lens id (const . take maxi)
        len = liftM (\r -> lens length $ extendList r . min maxi) $ readRef range
        sel = runR $ liftM (filter snd) $ readRef list
    return $ Notebook
        [ (,) "Editor" $ vcat
            [ hcat
                [ Entry $ joinRef $ liftM (\k -> showLens . k % list) len
                , smartButton (return "+1") (modL' len (+1))      list
                , smartButton (return "-1") (modL' len (+(-1)))   list
                , smartButton (toFree $ liftM (("DeleteAll " ++) . show) $ runR len >>= \k -> runR $ readRef $ k % list) (modL' len $ const 0) list
                , Button (return "undo") $ toFree undo
                , Button (return "redo") $ toFree redo
                ]
            , hcat
                [ sbutton (return "+1")         (map $ mapFst (+1))           list
                , sbutton (return "-1")         (map $ mapFst (+(-1)))        list
                , sbutton (return "sort")       (sortBy (compare `on` fst))   list
                , sbutton (return "SelectAll")  (map $ mapSnd $ const True)   list
                , sbutton (return "SelectPos")  (map $ \(a,_) -> (a, a>0))    list
                , sbutton (return "SelectEven") (map $ \(a,_) -> (a, even a)) list
                , sbutton (return "InvertSel")  (map $ mapSnd not)            list
                , sbutton (toFree $ liftM (("DelSel " ++) . show . length) sel) (filter $ not . snd) list
                , smartButton (return "CopySel") (modL_ safe $ concatMap $ \(x,b) -> (x,b): [(x,False) | b]) list
                , sbutton (return "+1 Sel")     (map $ mapSel (+1))           list
                , sbutton (return "-1 Sel")     (map $ mapSel (+(-1)))        list
                ]
            , Label $ toFree $ liftM (("Sum: " ++) . show . sum . map fst) sel
            , Action $ listEditor def (itemEditor list) list
            ]
        , (,) "Settings" $ hcat
            [ Label $ return "Create range"
            , Checkbox range
            ]
        ]
 where
    itemEditor list i r = return $ hcat
        [ Label $ return $ show (i+1) ++ "."
        , Entry $ showLens . fstLens % r
        , Checkbox $ sndLens % r
        , Button (return "Del")  $ return $ Just $ modRef list (\xs -> take i xs ++ drop (i+1) xs)
        , Button (return "Copy") $ return $ Just $ modRef list (\xs -> take (i+1) xs ++ drop i xs) ]

    modL' mr f b = do
        r <- runR mr
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


listEditor :: ExtRef m => a -> (Int -> Ref m a -> C m (I m)) -> Ref m [a] -> C m (I m)
listEditor def ed = editor 0 where
  editor i r = liftM Action $ memoRead $ do
    q <- extRef r listLens (False, (def, []))
    t1 <- ed i $ fstLens . sndLens % q
    t2 <- editor (i+1) $ sndLens . sndLens % q
    return $ Cell True (liftM fst $ readRef q) $ \b -> vcat $ if b then [t1, t2] else []



