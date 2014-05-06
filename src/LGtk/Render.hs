{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
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
import Control.Lens hiding ((#), beside)
import Data.List
import Diagrams.Prelude
import Diagrams.BoundingBox
import Graphics.SVGFonts
import Data.Colour.SRGB
import Unsafe.Coerce

import Data.LensRef
import LGtk.Effects
import LGtk.Widgets

----------------------

pairs xs = zip xs (tail xs)


-------------------------------------- Maybe type with another semigroup structure

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

whenMaybe' :: Monad m => (a -> m ()) -> Maybe' a -> m ()
whenMaybe' f (Just' a) = f a
whenMaybe' _ Nothing' = return ()

-------------------------------------- better clipBy

data X a = X a | Cancel | Z

instance Monoid (X a) where
    mempty = Z
    mappend = (<>)

instance Semigroup (X a) where
    x <> Z = x
    Z <> x = x
    _ <> Cancel = Cancel
    Cancel <> _ = Cancel

clipBy' p d = fmap unX (fmap X d # clipBy p # withEnvelope p  <> fmap flipp (stroke p # lw 0 # value Cancel))
  where
    flipp Cancel = Z
    flipp Z = Cancel

    unX (X a) = a
    unX Cancel = mempty

--------------------------------------------------------- Identifiers

newtype Id = Id [Int]
    deriving Eq

type WithId = StateT Id

firstId :: Id
firstId = Id []

newId :: Monad m => WithId m Id
newId = do
  x@(Id (i:is)) <- get
  put $ Id $ i+1:is
  return x

newIds :: Monad m => Id -> WithId m a -> m a
newIds (Id i) = flip evalStateT $ Id (0:i)

---------------------------------------------------------

type FocFun m = [KeyModifier] -> String -> Maybe Char -> m ()

type Foc m = (FocFun m, m ())

type EventHandle m = MouseEvent () -> (Maybe' (m ()), Maybe' (Foc m), Maybe' (Id, Id))

type KeyHandle m = (m (), FocFun m, m (), Id)

-- compiled widget
data CWidget m
    = forall x . Eq x
    => CWidget (ReadRef m ([KeyHandle m], x)) (x -> [Id] -> Id -> Dia (EventHandle m))

------------------

value_ :: Monad m => m () -> Maybe' (Foc m) -> Id -> Id -> Dia Any -> Dia (EventHandle m)
value_ a c i i' = value f where
    f (Click _) = (Just' a, c, Just' (i, i'))
    f (MoveTo _) = (Nothing', Nothing', Just' (i, i'))
    f _ = mempty

adjustFoc :: EffRef m => Ref m (Foc (Modifier m)) -> Modifier m ()
adjustFoc foc = join $ readRef' $ _2 `lensMap` foc

-----------------

inCanvas :: forall m . EffIORef m => Int -> Int -> Double -> Widget m -> Widget m
inCanvas width height scale w = do
    let df = (\_ _ _ -> return (), return ())
    foc <- newRef df
    (i, bhr) <- newIds firstId $ liftM2 (,) newId $ tr (fromIntegral width / scale) w
    hi <- newRef ([i], i)
    case bhr of
       CWidget b render -> do
        let handle (a, bb, c) = do
                whenMaybe' id a
                whenMaybe' h2 bb
                whenMaybe' h3 c
                whenMaybe' h4 (liftM2 (,) bb c)

            h2 m = do
                adjustFoc foc
                writeRef foc m
            h3 (i,_) = writeRef (_1 `lensMap` hi) [i]
            h4 (_,(_,i)) = do
                writeRef (_2 `lensMap` hi) i

            moveFoc f = do
                (_, j) <- readRef' hi
                (xs, _) <- liftReadRef b
                let (a,bb,c,d) = maybe (head xs) snd $ find (\((_,_,_,x),_) -> x == j) $ pairs $ (if f then id else reverse) (xs ++ xs)
                a >> h2 (bb,c) >> h4 (undefined, (d,d))

            handleEvent (Click (MousePos p f)) = handle $ f $ Click $ MousePos p ()  :: Modifier m ()
            handleEvent (MoveTo (MousePos p f)) = handle $ f $ MoveTo $ MousePos p ()
            handleEvent (KeyPress [] "Tab" _) = moveFoc True
            handleEvent (KeyPress [c] "Tab" _) | c == ControlModifier = moveFoc False
            handleEvent (KeyPress m n c) = do
                (f,_) <- readRef' foc
                f m n c
            handleEvent LostFocus = adjustFoc foc
            handleEvent _ = return ()
        return $ Canvas width height scale handleEvent (liftM2 (,) (readRef hi) $ liftM snd b) $ \((is,is'), x) -> 
              render x is is' # alignT # alignL # translate (r2 (-scale/2,scale/2* fromIntegral height / fromIntegral width))
              <> rect scale (scale / fromIntegral width * fromIntegral height)
                    # value_ (return ()) (Just' df) i i

focWidth = 0.1

text__ :: Double -> Double -> String -> ((Double :& Double), Dia Any)
{-
text_ s = (coords $ boxExtents (boundingBox t) + r2 (0.2, 0.2) , t) where
    t = textSVG s 1.8 # stroke # fc black  
-}
text__ ma mi s = ((max mi (min ma $ fromIntegral (length s) * 2/3) :& 1), text s)

defcolor = sRGB 0.95 0.95 0.95

tr :: forall m . EffIORef m => Double -> Widget m -> WithId m (CWidget (Modifier m))
tr sca w = do
    w' <- lift w
    case w' of
        Label r -> do
            let render bv _ _ = ((rect x y # lw 0 <> te) # clipped (rect x y)) # value mempty
                     where ((x :& y), te) = text__ 15 5 bv
            return $ CWidget (liftM ((,) []) r) render

        Button r sens col a -> do
            i <- newId

            let ff _ _ (Just ' ') = a ()
                ff _ _ (Just '\n') = a ()
                ff _ _ _ = return ()

                col' = maybe (return black) id col

                render (bv, se, color) is is' =
                     (te # fc (if se then color else gray)
                  <> roundedRect x y 0.3 # fc (if i `elem` is && se then yellow else defcolor)
                         # (if is' == i && se then lc yellow . lw focWidth else lc black . lw 0.02)
                     )
                        # (if se then value_ (a ()) (Just' (ff, return ())) i i else value mempty)
                        # clipBy' (rect (x+0.1) (y+0.1)) # freeze # frame 0.1
                   where ((x :& y), te) = text__ 15 3 bv
            return $ CWidget (liftM3 (\r se c -> ([(return (), ff, return (), i) | se], (r,se,c))) r sens col') render

        Entry (rs, rr) -> do
            i <- newId
--            s <- readRef rs
            j <- lift $ newRef (False, ("", ""))
            _ <- lift $ onChangeSimple rs $ \s -> asyncWrite 0 $ do
                writeRef (_2 `lensMap` j) (reverse s, "")

            let f _ (Just c) (a,b) = (c:a,b)
                f "BackSpace" _ (_:a,b) = (a,b)
                f "Delete" _ (a,_:b) = (a,b)
                f "Left" _ (c:a,b) = (a,c:b)
                f "Right" _ (a,c:b) = (c:a,b)
                f _ _ x = x

                ff _ _ (Just '\n') = do
                    (_, (a, b)) <- readRef' j
                    rr $ reverse a ++ b
                ff _ e f' = do
                    modRef (_2 `lensMap` j) $ f e f'

                text' (False,(a,b)) = reverse a ++ b
                text' (True,(a,b)) = reverse a ++ "|" ++ b

                fin = writeRef (_1 `lensMap` j) True
                fout = writeRef (_1 `lensMap` j) False

                render bv is is' = 
                  (  te # clipped (rect x y) # value mempty
                  <> rect x y # (if i `elem` is then fc yellow else id)
                         # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                         # value_ fin (Just' (ff, fout)) i i
                  ) # freeze # frame 0.1
                   where ((x :& y), te) = text__ 7 5 $ text' bv
            return $ CWidget (liftM ((,) [(fin, ff, fout, i)]) (readRef j)) render

        Checkbox (bs, br) -> do
            i <- newId

            let ff _ _ (Just ' ') = liftReadRef bs >>= br . not
                ff _ _ _ = return ()

                render bv is is' = 
                    (
                       (if bv then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # lw 0.15
                    <> rect 1 1 # value_ (br (not bv)) (Just' (ff, return ())) i i
                                # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                    ) # freeze # frame 0.1
            return $ CWidget (liftM ((,) [(return (), ff, return (), i)]) bs) render

        Cell r f -> do
            i <- newId
            r' <- lift $ onChange r $ \x -> do   
                     h <- f (newIds i . tr sca) x
                     return $ do
                       hv <- h
                       return $ case hv of
                         CWidget rr render -> do
                           (es, rrv) <- rr
                           return $ (es, UnsafeEqWrap (x, rrv) $ render rrv)
            return $ CWidget (join r') $ \(UnsafeEqWrap _ d) is is' -> d is is'

        List layout ws -> liftM (foldr conc2 nil) $ mapM (tr sca) ws
          where
            nil :: ExtRef n => CWidget n
            nil = CWidget (return ([],())) mempty

            conc2 (CWidget b r) (CWidget b' r')
              = CWidget (liftM2 (\(a,b)(c,d)->(a++c,(b,d))) b b') (\(x,y) -> liftM2 (liftM2 ff) (r x) (r' y))

            ff = case layout of
                Horizontal -> \a b -> a # alignT ||| b # alignT
                Vertical -> \a b -> a # alignL === b # alignL

        Canvas w h d r s f -> do

            i <- newId

            let ff x y z = r $ KeyPress x y z

                gg (Just' ls) (Click (MousePos p _)) = (Just' $ r (Click $ MousePos p ls), Just' (ff, r LostFocus), Just' (i,i))
                gg (Just' ls) (MoveTo (MousePos p _)) = (Just' $ r (MoveTo $ MousePos p ls), Nothing', Just' (i,i))
                gg _ _ = mempty

                wi = fromIntegral w / sca
                hi = fromIntegral h / sca

                render bv _is _is' = (fmap gg (fmap Just' (f bv # freeze) # scale ((fromIntegral w / d) / sca)
                                            # clipBy' (rect wi hi))
                   <> rect wi hi # value mempty # lw 0.02
                         )  # freeze  # frame 0.1

            return $ CWidget (liftM ((,) []) s) render

        Combobox xs (bs, br) -> do
            let n = length xs
            iss <- replicateM n newId
            ii <- newId

            let -- ff ind _ _ (Just ' ') = br ind
                br' ind = br (ind `mod` n)
                br'' f = liftReadRef bs >>= br' . f 
                ff _ "Down" _ = br'' (+1)
                ff _ "Up" _ = br'' (+(-1))
                ff _ _ _ = return ()

                render bv is is' = vcat (zipWith3 g [0..] iss xs) # freeze # frame 0.1
                  where
                    g ind i txt = (
                       (if bv == ind then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # translate (r2 (x / 2,0)) # lw 0.15
                      <> te # clipped (rect x y) # value mempty
                      <> rect x y # (if i `elem` is then fc yellow else id)
                             # (if is' == ii then lc yellow . lw focWidth else lc black . lw 0.02)
                             # value_ (br ind) (Just' (ff, return ())) i ii
                            )  # frame 0.02

                     where ((x :& y), te) = text__ 15 3 txt

            return $ CWidget (liftM ((,) [(return (), ff, return (), ii)]) bs) render

        Notebook' br xs -> do
            let (names, wis) = unzip xs
                n = length names
            iss <- replicateM n newId
            ii <- newId
            ir <- lift $ newRef (0 :: Int)
            wisv <- mapM (tr sca) wis

            wr <- lift $ onChangeSimple (readRef ir) $ \x -> return $ case wisv !! x of
                         CWidget rr render -> do
                           (es, rrv) <- rr
                           return $ (es, UnsafeEqWrap (x, rrv) $ render rrv)


            let br' :: Int -> Modifier m ()
                br' ind = br ind' >> writeRef ir ind' where ind' = ind `mod` n
                br'' f = readRef' ir >>= br' . f 
                ff _ "Left" _ = br'' (+(-1))
                ff _ "Right" _ = br'' (+ 1)
                ff [AltModifier] _ (Just c) | Just i <- ind c = br'' (const i)
                ff _ _ _ = return ()

                ind c | 0 <= i && i < n = Just i
                      | otherwise = Nothing
                  where i = fromEnum c - fromEnum '1'

            do
                let
                    render (bv, UnsafeEqWrap _ d) is is' =
                        vcat
                            [ strutY 0.1
                            , alignL $ line 0.2
                               ||| hcat (intersperse (line 0.1) $ zipWith3 g [0..] iss names)
                               ||| line 100
                            , strutY 0.2
                            , alignL $ d is is'
                            ] # freeze
                      where
                        line dx = fromOffsets [r2 (dx,0)] # strokeLine # translate (r2 (0,-0.5)) # value mempty # lcw

                        lcw = if is' == ii then lc yellow . lw focWidth else lc black . lw 0.02

                        g ind i txt =
                             te # clipped (rect x y) # value mempty
                                 # fc black
                          <> bez # value mempty
                                 # lcw
                          <> (if bv == ind then mempty else line x # translate (r2 (-x/2, 0))) # lcw
                          <> bez' # closeLine # strokeLoop  # translate (r2 (-x/2,-y/2)) # lw 0
                                 # (if (bv == ind) then value mempty else value_ (br' ind) (Just' (ff, return ())) i ii)
                                 # (if bv /= ind then fc (if i `elem` is then yellow else defcolor) else id)
                         where ((x_ :& y), te) = text__ 10 3 txt
                               x = x_ + 2
                               bez = fromSegments [ bezier3 (r2 (0.7,0)) (r2 (0.3,1)) (r2 (1,1))
                                          , straight (r2 (x - 2, 0))
                                          , bezier3 (r2 (0.7,0)) (r2 (0.3,-1)) (r2 (1,-1))
                                          ] # translate (r2 (-x/2,-y/2))
                               bez' = fromSegments [ bezier3 (r2 (0.7,0)) (r2 (0.3,1)) (r2 (1,1))
                                          , straight (r2 (x - 2, 0))
                                          , bezier3 (r2 (0.7,0)) (r2 (0.3,-1)) (r2 (1,-1))
                                          ]

                return $ CWidget (liftM2 (\iv (ls,vv) ->
                    ( (return (), ff, return (), ii): ls
                    , (iv, vv)
                    )) (readRef ir) (join wr)) render

        Scale _ _ _ _ -> error "sc"


--------------------

data UnsafeEqWrap b = forall a . Eq a => UnsafeEqWrap a b
instance Eq (UnsafeEqWrap b) where
    UnsafeEqWrap a _ == UnsafeEqWrap b _ = a == unsafeCoerce b


