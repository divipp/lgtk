{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module LGtk.Canvas where

import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((#))
import Diagrams.Prelude hiding (vcat, hcat, Dynamic)
import Data.Colour.SRGB
import Unsafe.Coerce

import LGtk
import GUI.Gtk.Structures

--------------------------------------

data Maybe' a = Just' a | Nothing'

instance Semigroup (Maybe' a) where
    _ <> Just' a = Just' a
    a <> Nothing' = a

instance Monoid (Maybe' a) where
    mempty = Nothing'
    mappend = (<>)

instance Monad Maybe' where
    return = Just'
    Just' x >>= f = f x
    _ >>= _ = Nothing'



--------------------

data Cache a b = Cache a b
instance Eq a => Eq (Cache a b) where
    Cache a _ == Cache b _ = a == b

---------------

data Dy = forall a . Eq a => Wrap a

instance Eq Dy where
  Wrap a == Wrap b  = a == unsafeCoerce b

--------------------

data X a = X a | Cancel | Z

instance Monoid (X a) where
    mempty = Z
    mappend = (<>)

instance Semigroup (X a) where
    x <> Z = x
    Z <> x = x
    _ <> Cancel = Cancel
    Cancel <> _ = Cancel

---------

clipBy' p d = fmap unX (fmap X d # clipBy p  <> fmap flipp (stroke p # lw 0 # value Cancel))
  where
    flipp Cancel = Z
    flipp Z = Cancel

    unX (X a) = a
    unX Cancel = mempty

------------------

data WW m = forall x . Eq x => WW (RefStateReader m ([(m (), FocFun m, m (), Pos)], x)) (x -> [Pos] -> Pos -> Dia (Mon m))

type WW' m = WW (RefState (RefCore m))

type Pos = [Int]

newId :: MonadState Pos m => m Pos
newId = do
  (i:is) <- get
  put $ i+1:is
  return $ i:is

type Mon m = MouseEvent () -> (Maybe' (m ()), Maybe' (Foc m), Maybe' Pos)

type FocFun m = [Modifier] -> String -> Maybe Char -> m ()

value_ a c b = value f where
    f (Click _) = (Just' a, c, Just' b)
    f (MoveTo _) = (Nothing', Nothing', Just' b)
    f _ = mempty

-------------------------

type Foc m = (FocFun m, m ())
type Foc' m = Foc (RefState (RefCore m))

adjustFoc foc = join $ liftRefStateReader $ readRef $ _2 `lensMap` foc

-----------------

app x f = f x

mb f (Just' a) = f a
mb _ Nothing' = return ()

inCanvas :: EffRef m => Int -> Int -> Double -> Widget m -> Widget m
inCanvas width height scale w = do
    let df = (\_ _ _ -> return (), return ())
    foc <- newRef df
    (i, bhr) <- flip evalStateT [0] $ liftM2 (,) newId $ tr (fromIntegral width / scale) w
    hi <- newRef ([i], i)
    tab <- newRef 0
    case bhr of
       WW b render -> do
        let handle (a, b, c) = mb id a >> mb h2 b >> mb h3 c >> mb h4 (liftM2 (,) b c)

            h2 m = adjustFoc foc >> writeRef foc m
            h3 i = writeRef (_1 `lensMap` hi) [i]
            h4 (_,i) = writeRef (_2 `lensMap` hi) i

            moveFoc f = do
                modRef tab f
                j <- liftRefStateReader $ readRef tab
                (xs, _) <- liftRefStateReader b
                let (a,b,c,d) = xs !! (j `mod` length xs)
                a >> h2 (b,c) >> h4 (undefined, d)

            handleEvent (Click (MousePos p f)) = handle $ f $ Click $ MousePos p ()
            handleEvent (MoveTo (MousePos p f)) = handle $ f $ MoveTo $ MousePos p ()
            handleEvent (KeyPress [] _ "Tab" _) = moveFoc (+1)
            handleEvent (KeyPress [Control] _ "Tab" _) = moveFoc (+(-1))
            handleEvent (KeyPress m _ n c) = do
                (f,_) <- liftRefStateReader $ readRef foc
                f m n c
            handleEvent LostFocus = adjustFoc foc
            handleEvent _ = return ()
        canvas width height scale handleEvent (liftM2 (,) (readRef hi) $ liftM snd b) $ \((is,is'), x) -> 
              render x is is' # translate (r2 (-scale/4,scale/4* fromIntegral height / fromIntegral width))
              <> rect scale (scale / fromIntegral width * fromIntegral height)
                    # value_ (return ()) (Just' df) i


tr :: ExtRef m => Double -> Widget m -> StateT Pos m (WW' m)
tr sca w = do
    w' <- lift w
    case w' of
        Label (Send r) -> do
            let render bv _ _ = ((rect 5 1 # lw 0 <> text bv) # clipBy (rect 5 1)) # value mempty
            return $ WW (liftM ((,) []) r) render

        Button (Send r) _ _ a -> do
            i <- newId

            let ff _ _ (Just ' ') = a ()
                ff _ _ _ = return ()

                render bv is is' =
                     (text bv
                  <> roundedRect 4.9 0.9 0.3 # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                         # (if is' == i then lc yellow . lw 0.05 else lc black . lw 0.02)
                     )
                        # value_ (a ())
                                 (Just' (ff, return ()))
                                 i
                        # clipBy' (rect 5 1)
            return $ WW (liftM ((,) [(return (), ff, return (), i)]) r) render

        Entry (Send rs, rr) -> do
            i <- newId
            j <- lift $ newRef (False, 0)

            let f _ (Just c) (a,b) = (c:a,b)
                f "BackSpace" _ (_:a,b) = (a,b)
                f "Delete" _ (a,_:b) = (a,b)
                f "Left" _ (c:a,b) = (a,c:b)
                f "Right" _ (a,c:b) = (c:a,b)
                f _ _ x = x

                ff _ e f' = do
                    (_, x) <- liftRefStateReader $ readRef j
                    s <- liftRefStateReader rs
                    let  (a,b) = splitAt x s
                         (a', b') = f e f' (reverse a,b)
                    rr $ reverse a' ++ b'
                    writeRef (_2 `lensMap` j) $ length a'

                text' ((False,_),s) = s
                text' ((True,i),s) = a ++ "|" ++ b where (a,b) = splitAt i s

                fin = writeRef (_1 `lensMap` j) True
                fout = writeRef (_1 `lensMap` j) False

                render bv is is' = 
                     text (text' bv) # clipBy (rect 5 1) # value mempty
                  <> rect 5 1 # fc (if i `elem` is then yellow else white)
                         # (if is' == i then lc yellow . lw 0.05 else lc black . lw 0.02)
                         # value_ fin (Just' (ff, fout)) i
            return $ WW (liftM ((,) [(fin, ff, fout, i)]) (liftM2 (,) (readRef j) rs)) render

        Checkbox (Send bs, br) -> do
            i <- newId

            let ff _ _ (Just ' ') = liftRefStateReader bs >>= br . not
                ff _ _ _ = return ()

                render bv is is' = 
                       (if bv then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # lw 0.15
                    <> rect 1 1 # value_ (br (not bv)) (Just' (ff, return ())) i
                                # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                # (if is' == i then lc yellow . lw 0.05 else lc black . lw 0.02)
            return $ WW (liftM ((,) [(return (), ff, return (), i)]) bs) render

        Cell (Send r) f -> do
            i <- newId
            r' <- lift $ lazyExtRef r $ \x -> do   
                     h <- f (flip evalStateT (0:i) . tr sca) x
                     return $ do
                       hv <- h
                       return $ case hv of
                         WW rr render -> do
                           (es, rrv) <- rr
                           return $ (es, Cache (x, Wrap rrv) (render rrv))
            return $ WW (join r') $ \(Cache _ d) is is' -> d is is'

        List layout ws -> liftM (foldr conc2 nil) $ mapM (tr sca) ws
          where
            nil = WW (return ([],())) mempty

            conc2 (WW b r) (WW b' r')
              = WW (liftM2 (\(a,b)(c,d)->(a++c,(b,d))) b b') (\(x,y) -> liftM2 (liftM2 ff) (r x) (r' y))

            ff = case layout of
                Horizontal -> (|||)
                Vertical -> (===)

        Canvas w h d r (Send s) f -> do

            i <- newId

            let ff x y z = r $ KeyPress x undefined y z

                gg (Just' ls) (Click (MousePos p _)) = (Just' $ r (Click $ MousePos p ls), Just' (ff, r LostFocus), Just' i)
                gg (Just' ls) (MoveTo (MousePos p _)) = (Just' $ r (MoveTo $ MousePos p ls), Nothing', Just' i)
                gg _ _ = mempty

                wi = fromIntegral w / sca
                hi = fromIntegral h / sca

                render bv _is _is' = fmap gg (fmap Just' (f bv) # scale ((fromIntegral w / d) / sca) # clipBy' (rect wi hi))
                   <> rect wi hi # value mempty
                           # lw 0.02

            return $ WW (liftM ((,) []) s) render


----------------------------------------------------------------------------

main :: IO ()
main = runWidget $ do
    t <- newRef $ iterate (Node Leaf) Leaf !! 5
    i <- newRef 0
    s <- newRef "x"
    s' <- newRef "y"
    let x = vcat
            [ hcat
                [ label $ readRef i >>= \i -> return $ show i ++ "hello"
                , button_ (return "+1") (return True) $ modRef i (+1)
                ]
            , hcat
                [ entry s
                , entry s
                ]
            , hcat
                [ entry s'
                , entry s'
                ]
            , join $ tEditor3 t
            ]

    hcat
        [ inCanvas 600 400 30 $ hcat [ inCanvas 200 300 15 $ vcat [x, inCanvas 100 100 15 x], x], x ]


-- | Binary tree shapes
data T
    = Leaf
    | Node T T
        deriving Show

-- | Lens for @T@
tLens :: Lens' (Bool, (T, T)) T
tLens = lens get set where
    get (False, _)     = Leaf
    get (True, (l, r)) = Node l r
    set (_, x) Leaf  = (False, x)
    set _ (Node l r) = (True, (l, r))

-- | @T@ editor with checkboxes, given directly
tEditor3 :: EffRef m => Ref m T -> m (Widget m)
tEditor3 r = do
    q <- extRef r tLens (False, (Leaf, Leaf))
    return $ hcat
        [ checkbox $ _1 `lensMap` q
        , cell (liftM fst $ readRef q) $ \b -> case b of
            False -> empty
            True -> do
                t1 <- tEditor3 $ _2 . _1 `lensMap` q
                t2 <- tEditor3 $ _2 . _2 `lensMap` q
                vcat [t1, t2]
        ]
