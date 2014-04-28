{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module LGtk.Render
    ( inCanvas
    ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((#))
import Diagrams.Prelude
import Diagrams.BoundingBox
import Graphics.SVGFonts
import Data.Colour.SRGB
import Unsafe.Coerce

--import LGtk
import Data.LensRef
import LGtk.Widgets

--------------------------------------

data Maybe' a = Just' a | Nothing'

instance Semigroup (Maybe' a) where
    _ <> Just' a = Just' a
    a <> Nothing' = a

instance Monoid (Maybe' a) where
    mempty = Nothing'
    mappend = (<>)

instance Monad Maybe' where
    return = pure
    Just' x >>= f = f x
    _ >>= _ = Nothing'

instance Applicative Maybe' where
    pure = Just'
    Just' f <*> Just' x = Just' $ f x
    _ <*> _ = Nothing'

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' x) = Just' $ f x

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

data WW m = forall x . Eq x => WW (ReadRef m ([(m (), FocFun m, m (), Pos)], x)) (x -> [Pos] -> Pos -> Dia (Mon m))

type WW' m = WW (Modifier m)

type Pos = [Int]

newId :: MonadState Pos m => m Pos
newId = do
  (i:is) <- get
  put $ i+1:is
  return $ i:is

type Mon m = MouseEvent () -> (Maybe' (m ()), Maybe' (Foc m), Maybe' Pos)

type FocFun m = [KeyModifier] -> String -> Maybe Char -> m ()

value_ a c b = value f where
    f (Click _) = (Just' a, c, Just' b)
    f (MoveTo _) = (Nothing', Nothing', Just' b)
    f _ = mempty

-------------------------

type Foc m = (FocFun m, m ())
type Foc' m = Foc (Modifier m)

adjustFoc :: (EffRef m) => Ref m (Foc' m) -> Modifier m ()
adjustFoc foc = join $ liftRefStateReader' $ readRef $ _2 `lensMap` foc

-----------------

mb f (Just' a) = f a
mb _ Nothing' = return ()

inCanvas :: forall m . EffRef m => Int -> Int -> Double -> Widget m -> Widget m
inCanvas width height scale w = do
    let df = (\_ _ _ -> return (), return ())
    foc <- newRef df
    (i, bhr) <- flip evalStateT [0] $ liftM2 (,) newId $ tr (fromIntegral width / scale) w
    hi <- newRef ([i], i)
    tab <- newRef (0 :: Int)
    case bhr of
       WW b render -> do
        let handle (a, bb, c) = mb id a >> mb h2 bb >> mb h3 c >> mb h4 (liftM2 (,) bb c)

            h2 m = adjustFoc foc >> writeRef' foc m
            h3 i = writeRef' (_1 `lensMap` hi) [i]
            h4 (_,i) = writeRef' (_2 `lensMap` hi) i

            moveFoc f = do
                j <- liftRefStateReader' $ readRef tab
                (xs, _) <- liftRefStateReader' b
                let j' = f j `mod` length xs
                writeRef' tab j'
                let (a,bb,c,d) = xs !! j'
                a >> h2 (bb,c) >> h4 (undefined, d)

            handleEvent (Click (MousePos p f)) = handle $ f $ Click $ MousePos p ()  :: Modifier m ()
            handleEvent (MoveTo (MousePos p f)) = handle $ f $ MoveTo $ MousePos p ()
            handleEvent (KeyPress [] "Tab" _) = moveFoc (+1)
            handleEvent (KeyPress [c] "Tab" _) | c == ControlModifier = moveFoc (+(-1))
            handleEvent (KeyPress m n c) = do
                (f,_) <- liftRefStateReader' $ readRef foc
                f m n c
            handleEvent LostFocus = adjustFoc foc
            handleEvent _ = return ()
        return $ Canvas width height scale handleEvent (liftM2 (,) (readRef hi) $ liftM snd b) $ \((is,is'), x) -> 
              render x is is' # alignT # alignL # translate (r2 (-scale/2,scale/2* fromIntegral height / fromIntegral width))
              <> rect scale (scale / fromIntegral width * fromIntegral height)
                    # value_ (return ()) (Just' df) i

focWidth = 0.2

text__ :: Double -> Double -> String -> ((Double :& Double), Dia Any)
{-
text_ s = (coords $ boxExtents (boundingBox t) + r2 (0.2, 0.2) , t) where
    t = textSVG s 1.8 # stroke # fc black  
-}
text__ ma mi s = ((max mi (min ma $ fromIntegral (length s) * 2/3) :& 1), text s)

tr :: forall m . EffRef m => Double -> Widget m -> StateT Pos m (WW' m)
tr sca w = do
    w' <- lift w
    case w' of
        Label r -> do
            let render bv _ _ = ((rect x y # lw 0 <> te) # clipped (rect x y)) # value mempty
                     where ((x :& y), te) = text__ 15 5 bv
            return $ WW (liftM ((,) []) r) render

        Button r sens _ a -> do
            i <- newId

            let ff _ _ (Just ' ') = a ()
                ff _ _ _ = return ()

                render (bv, se) is is' =
                     (te # fc (if se then black else gray)
                  <> roundedRect x y 0.3 # fc (if i `elem` is && se then yellow else sRGB 0.95 0.95 0.95)
                         # (if is' == i && se then lc yellow . lw focWidth else lc black . lw 0.02)
                     )
                        # (if se then value_ (a ()) (Just' (ff, return ())) i else value mempty)
                        # clipBy' (rect (x+0.1) (y+0.1)) # freeze # frame 0.1
                   where ((x :& y), te) = text__ 15 3 bv
            return $ WW (liftM (\(r,se) -> ([(return (), ff, return (), i) | se], (r,se))) $ liftM2 (,) r sens) render

        Entry (rs, rr) -> do
            i <- newId
            j <- lift $ newRef (False, 0)

            let f _ (Just c) (a,b) = (c:a,b)
                f "BackSpace" _ (_:a,b) = (a,b)
                f "Delete" _ (a,_:b) = (a,b)
                f "Left" _ (c:a,b) = (a,c:b)
                f "Right" _ (a,c:b) = (c:a,b)
                f _ _ x = x

                ff _ e f' = do
                    (_, x) <- liftRefStateReader' $ readRef j
                    s <- liftRefStateReader' rs
                    let  (a,b) = splitAt x s
                         (a', b') = f e f' (reverse a,b)
                    rr $ reverse a' ++ b'
                    writeRef' (_2 `lensMap` j) $ length a'

                text' ((False,_),s) = s
                text' ((True,i),s) = a ++ "|" ++ b where (a,b) = splitAt i s

                fin = writeRef' (_1 `lensMap` j) True
                fout = writeRef' (_1 `lensMap` j) False

                render bv is is' = 
                  (  te # clipped (rect x y) # value mempty
                  <> rect x y # fc (if i `elem` is then yellow else white)
                         # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                         # value_ fin (Just' (ff, fout)) i
                  ) # freeze # frame 0.1
                   where ((x :& y), te) = text__ 7 5 $ text' bv
            return $ WW (liftM ((,) [(fin, ff, fout, i)]) (liftM2 (,) (readRef j) rs)) render

        Checkbox (bs, br) -> do
            i <- newId

            let ff _ _ (Just ' ') = liftRefStateReader' bs >>= br . not
                ff _ _ _ = return ()

                render bv is is' = 
                    (
                       (if bv then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # lw 0.15
                    <> rect 1 1 # value_ (br (not bv)) (Just' (ff, return ())) i
                                # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                    ) # freeze # frame 0.1
            return $ WW (liftM ((,) [(return (), ff, return (), i)]) bs) render

        Cell (r) f -> do
            i <- newId
            r' <- lift $ onChange r $ \x -> do   
                     h <- f x
                     return $ do
                       hv <- flip evalStateT (0:i) . tr sca $ h
                       return $ case hv of
                         WW rr render -> do
                           (es, rrv) <- rr
                           return $ (es, Cache (x, Wrap rrv) (render rrv))
            return $ WW (join r') $ \(Cache _ d) is is' -> d is is'

        List layout ws -> liftM (foldr conc2 nil) $ mapM (tr sca) ws
          where
            nil :: ExtRef n => WW n
            nil = WW (return ([],())) mempty

            conc2 (WW b r) (WW b' r')
              = WW (liftM2 (\(a,b)(c,d)->(a++c,(b,d))) b b') (\(x,y) -> liftM2 (liftM2 ff) (r x) (r' y))

            ff = case layout of
                Horizontal -> \a b -> a # alignT ||| b # alignT
                Vertical -> \a b -> a # alignL === b # alignL

        Canvas w h d r (s) f -> do

            i <- newId

            let ff x y z = r $ KeyPress x y z

                gg (Just' ls) (Click (MousePos p _)) = (Just' $ r (Click $ MousePos p ls), Just' (ff, r LostFocus), Just' i)
                gg (Just' ls) (MoveTo (MousePos p _)) = (Just' $ r (MoveTo $ MousePos p ls), Nothing', Just' i)
                gg _ _ = mempty

                wi = fromIntegral w / sca
                hi = fromIntegral h / sca

                render bv _is _is' = (fmap gg (fmap Just' (f bv) # scale ((fromIntegral w / d) / sca) # clipBy' (rect wi hi))
                   <> rect wi hi # value mempty
                         )  # lw 0.02 # freeze  # frame 0.1

            return $ WW (liftM ((,) []) s) render

        Combobox xs (bs, br) -> do
            let n = length xs
            iss <- replicateM n newId

            let ff ind _ _ (Just ' ') = br ind
                ff _ _ _ _ = return ()

                render bv is is' = vcat (zipWith3 g [0..] iss xs) # freeze # frame 0.1
                  where
                    g ind i txt = (
                       (if bv == ind then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # translate (r2 (x / 2,0)) # lw 0.15
                      <> te # clipped (rect x y) # value mempty
                      <> rect x y # fc (if i `elem` is then yellow else white)
                             # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                             # value_ (br ind) (Just' (ff ind, return ())) i
                            )  # frame 0.02

                     where ((x :& y), te) = text__ 15 3 txt

            return $ WW (liftM ((,) [(return (), ff ind, return (), i) | (ind,i) <- zip [0..] iss]) bs) render

        Notebook' br xs -> do
            let (names, wis) = unzip xs
                n = length names
            iss <- replicateM n newId
            ir <- lift $ newRef (0 :: Int)

            let br' :: Int -> Modifier m ()
                br' ind = br ind' >> writeRef' ir ind' where ind' = ind `mod` n
                br'' f = readRef' ir >>= br' . f 
                ff ind _ _ (Just ' ') = br' ind
                ff _ _ "Left" _ = br'' (+(-1))
                ff _ _ "Right" _ = br'' (+ 1)
                ff _ _ _ _ = return ()

            ww <- tr sca $ return $ Cell (readRef ir) $ \iv -> return $ wis !! iv
            case ww of
              WW wr wf -> do

                let
                    render (bv,wv) is is' =
                        vcat
                            [ hcat (zipWith3 g [0..] iss names) # freeze # frame 0.1
                            , wf wv is is'
                            ]
                      where
                        g ind i txt =
--                           (if bv == ind then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
--                                    # lineCap LineCapRound else mempty) # value mempty # translate (r2 (x/2,0)) # lw 0.15
                             te # clipped (rect x y) # value mempty
                                 # fc (if i `elem` is then yellow else black)
                          <> rect x y # lw 0
                                 # value_ (br' ind) (Just' (ff ind, return ())) i
                          <> (if bv == ind then fromVertices [p2 (-x/2, -y/2), p2 (-x/2,y/2), p2 (x/2,y/2), p2 (x/2,-y/2)]
                                else rect x y)
                                 # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                                 # value mempty # frame 0.02
                         where ((x :& y), te) = text__ 10 3 txt

                return $ WW (liftM2 (\iv (ls,vv) ->
                    ( [ (return (), ff ind, return (), i) | (ind,i) <- zip [0..] iss] ++ ls
                    , (iv, vv)
                    )) (readRef ir) wr) render

        Scale _ _ _ _ -> error "sc"




