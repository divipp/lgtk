{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}
module LGtk.Render
    ( inCanvas
    ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((#), beside)
import Data.List
import Data.Maybe
import Diagrams.Prelude
--import Diagrams.BoundingBox
--import Graphics.SVGFonts
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
    deriving (Eq, Show)

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

-- how to handle keyboard events
type KeyHandler m = [KeyModifier] -> String -> Maybe Char -> m ()

-- focus enter action; keyboard event handler; focus leave action; id of the keyboard-focused widget
type KeyFocusHandler m = (m (), KeyHandler m, m (), Id)

type CapturedEventHandler m = MouseEvent () -> m Bool

-- for each mouse event:
--  - what to do
--  - the new keyboard focus handler
--  - the id of the mouse-focused widget
type EventHandler m = MouseEvent () -> Maybe' (m (), Maybe (CapturedEventHandler m), Maybe (KeyFocusHandler m), Id)

-- compiled widget
data CWidget m
    = forall x . Eq x
    => CWidget (ReadRef m (([KeyFocusHandler m], [[KeyFocusHandler m]]), x)) (x -> [Id] -> Id -> Dia (EventHandler m))

------------------

value_ :: Monad m => m () -> KeyFocusHandler m -> Id -> Dia Any -> Dia (EventHandler m)
value_ a c i = value f where
    f (Click _) = Just' (a, Nothing, Just c, i)
    f (MoveTo _) = Just' (return (), Nothing, Nothing, i)
    f _ = mempty

adjustFoc :: EffRef m => Ref m (KeyFocusHandler (Modifier m)) -> Modifier m ()
adjustFoc foc = join $ readRef' $ _3 `lensMap` foc

-----------------

[] !!! _ = Nothing
_ !!! n | n < 0 = Nothing
(x:_) !!! 0 = Just x
(_:xs) !!! n = xs !!! (n-1 :: Int)

[x] !!!! _ = x
(x:_) !!!! n | n <= 0 = x
(_:xs) !!!! n = xs !!!! (n-1 :: Int)

inCanvas :: forall m . (EffIORef m, MonadFix m) => Int -> Int -> Double -> Widget m -> Widget m
inCanvas width height scale w = mdo

    let i = firstId

    foc <- newRef df

    -- captured event handler
    capt <- newRef Nothing

    -- mouse-focused widget id
    hi <- newRef [i]

    let df = (return (), dkh, return (), i)

        calcPos i jss = listToMaybe [(a,b) | (a,js) <- zip [0..] jss, (b, j) <- zip [0..] js, i == j]

        changeFoc f = do
            (_, _, _, j) <- readRef' foc
            (_, xss_) <- liftReadRef bb
            let xss = filter (not . null) xss_
            if null xss
              then return ()
              else do
                let mp = calcPos j $ map (map (\(_,_,_,i)->i)) xss
                    handle (a,bb,c,d) = do
                        a >> h2 (a,bb,c,d)
                    ((a,_), (a',b')) = maybe ((0,0), (0,0)) (\x -> (x, f x)) mp
                if a == a'
                  then maybe (return ()) handle $ (xss !! a') !!! b'
                  else maybe (return ()) handle $ fmap (!!!! b') (xss !!! a')

        dkh [] "Up"     _ = changeFoc $ \(a,b) -> (a-1,b)
        dkh [] "Down"   _ = changeFoc $ \(a,b) -> (a+1,b)
        dkh [] "Left"   _ = changeFoc $ \(a,b) -> (a,b-1)
        dkh [] "Right"  _ = changeFoc $ \(a,b) -> (a,b+1)
        dkh _ _ _ = return ()

        h2 m = do
            adjustFoc foc
            writeRef foc m

    -- compiled widget
    bhr <- newIds firstId $ tr (fromIntegral width / scale) dkh w

    let bb = case bhr of
           CWidget b _ -> liftM fst b

    case bhr of
       CWidget b render -> do
        
        let handle_ Nothing' = return ()
            handle_ (Just' (a, cap, bb, i)) = do
                a
                maybe (return ()) (writeRef capt . Just) cap
                maybe (return ()) h2 bb
                writeRef hi [i]

            handle f x = do
                m <- readRef' capt
                case m of
                    Nothing -> handle_ $ f x
                    Just f -> do
                        b <- f x
                        when (not b) $ writeRef capt Nothing

            moveFoc f = do
                (_, _, _, j) <- readRef' foc
                ((xs, _), _) <- liftReadRef b
                let (a,bb,c,d) = maybe (head xs) snd $ find (\((_,_,_,x),_) -> x == j) $ pairs $ (if f then id else reverse) (xs ++ xs)
                a >> h2 (a,bb,c,d)

            handleEvent (Release (MousePos p f)) = handle f $ Release $ MousePos p ()  :: Modifier m ()
            handleEvent (Click (MousePos p f)) = handle f $ Click $ MousePos p ()  :: Modifier m ()
            handleEvent (MoveTo (MousePos p f)) = handle f $ MoveTo $ MousePos p ()
            handleEvent (KeyPress [] "Tab" _) = moveFoc True
            handleEvent (KeyPress [c] "Tab" _) | c == ControlModifier = moveFoc False
            handleEvent (KeyPress m n c) = do
                (_,f,_,_) <- readRef' foc
                f m n c
            handleEvent LostFocus = adjustFoc foc
            handleEvent _ = return ()
        return $ Canvas width height scale handleEvent (liftM3 (,,) (readRef hi) (readRef $ _4 `lensMap` foc) $ liftM snd b) $
            \(is, is', x) -> 
              render x is is' # alignT # alignL # translate (r2 (-scale/2,scale/2* fromIntegral height / fromIntegral width))
              <> rect scale (scale / fromIntegral width * fromIntegral height)
                    # value_ (return ()) df i

focWidth = 0.1

text__ :: Double -> Double -> String -> ((Double :& Double), Dia Any)
{-
text_ s = (coords $ boxExtents (boundingBox t) + r2 (0.2, 0.2) , t) where
    t = textSVG s 1.8 # stroke # fc black  
-}
text__ ma mi s = ((max mi (min ma $ fromIntegral (length s) * 2/3) :& 1), text s)

defcolor = sRGB 0.95 0.95 0.95

tr  :: forall m . EffIORef m
    => Double
    -> KeyHandler (Modifier m)
    -> Widget m
    -> WithId m (CWidget (Modifier m))
tr sca dkh w = do
    w' <- lift w
    case w' of
        Label r -> do
            let render bv _ _ = ((rect x y # lw 0 <> te) # clipped (rect x y)) # value mempty
                     where ((x :& y), te) = text__ 25 5 bv
            return $ CWidget (liftM ((,) ([], [])) r) render

        Button r sens col a -> do
            i <- newId

            let ff [] _ (Just ' ') = a ()
                ff [] _ (Just '\n') = a ()
                ff a b c = dkh a b c
                kh = (return (), ff, return (), i)

                col' = maybe (return black) id col

                render (bv, se, color) is is' =
                     (te # fc (if se then color else gray)
                  <> roundedRect x y 0.3 # fc (if i `elem` is && se then yellow else defcolor)
                         # (if is' == i && se then lc yellow . lw focWidth else lc black . lw 0.02)
                     )
                        # (if se then value_ (a ()) kh i else value mempty)
                        # clipBy' (rect (x+0.1) (y+0.1)) # freeze # frame 0.1
                   where ((x :& y), te) = text__ 15 3 bv
            return $ CWidget (liftM3 (\r se c -> (([kh | se], [[kh] | se]), (r,se,c))) r sens col') render

        Entry (rs, rr) -> do
            i <- newId
--            s <- readRef rs
            j <- lift $ newRef (False, ("", ""))
            _ <- lift $ onChangeSimple rs $ \s -> asyncWrite 0 $ do
                writeRef (_2 `lensMap` j) (reverse s, "")

            let f [] _ (Just c) (a,b) = Just (c:a,b)
                f [] "BackSpace" _ (_:a,b) = Just (a,b)
                f [] "Delete" _ (a,_:b) = Just (a,b)
                f [] "Left" _ (c:a,b) = Just (a,c:b)
                f [] "Right" _ (a,c:b) = Just (c:a,b)
                f _ _ _ _ = Nothing

                ff _ _ (Just '\n') = do
                    (_, (a, b)) <- readRef' j
                    rr $ reverse a ++ b
                ff m e f' = do
                    x <- readRef' (_2 `lensMap` j)
                    case f m e f' x of
                        Just x -> writeRef (_2 `lensMap` j) x
                        _ -> dkh m e f'

                text' (False,(a,b)) = reverse a ++ b
                text' (True,(a,b)) = reverse a ++ "|" ++ b

                fin = writeRef (_1 `lensMap` j) True
                fout = writeRef (_1 `lensMap` j) False
                kh = (fin, ff, fout, i)

                render bv is is' = 
                  (  te # clipped (rect x y) # value mempty
                  <> rect x y # (if i `elem` is then fc yellow else id)
                         # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                         # value_ fin kh i
                  ) # freeze # frame 0.1
                   where ((x :& y), te) = text__ 7 5 $ text' bv
            return $ CWidget (liftM ((,) ([kh],[[kh]])) (readRef j)) render

        Checkbox (bs, br) -> do
            i <- newId

            let ff [] _ (Just ' ') = liftReadRef bs >>= br . not
                ff a b c = dkh a b c
                kh = (return (), ff, return (), i)

                render bv is is' = 
                    (
                       (if bv then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # lw 0.15
                    <> rect 1 1 # value_ (br (not bv)) kh i
                                # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                # (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                    ) # freeze # frame 0.1
            return $ CWidget (liftM ((,) ([kh],[[kh]])) bs) render

        Cell r f -> do
            i <- newId
            r' <- lift $ onChange r $ \x -> do   
                     h <- f (newIds i . tr sca dkh) x
                     return $ do
                       hv <- h
                       return $ case hv of
                         CWidget rr render -> do
                           (es, rrv) <- rr
                           return $ (es, UnsafeEqWrap (x, rrv) $ render rrv)
            return $ CWidget (join r') $ \(UnsafeEqWrap _ d) is is' -> d is is'

        List layout ws -> liftM (foldr conc2 nil) $ mapM (tr sca dkh) ws
          where
            nil :: ExtRef n => CWidget n
            nil = CWidget (return (([],[]),())) mempty

            conc2 (CWidget b r) (CWidget b' r')
              = CWidget (liftM2 (\((a,a'),b)((c,c'),d)->((a++c,comb layout a' c'),(b,d))) b b') (\(x,y) -> liftM2 (liftM2 ff) (r x) (r' y))

            comb Vertical = (++)
            comb Horizontal = fx

            fx (a:as) (b:bs) = (a++b): fx as bs
            fx as bs = as ++ bs

            ff = case layout of
                Horizontal -> \a b -> a # alignT ||| b # alignT
                Vertical -> \a b -> a # alignL === b # alignL

        Canvas w h d r s f -> do

            i <- newId

            let ff x y z = r $ KeyPress x y z

                gg (Just' ls) (Click (MousePos p _)) = Just' (r (Click $ MousePos p ls), Nothing, Just (return (), ff, r LostFocus, i), i)
                gg (Just' ls) (MoveTo (MousePos p _)) = Just' (r (MoveTo $ MousePos p ls), Nothing, Nothing, i)
                gg _ _ = mempty

                wi = fromIntegral w / sca
                hi = fromIntegral h / sca

                render bv _is _is' = (fmap gg (fmap Just' (f bv # freeze) # scale ((fromIntegral w / d) / sca)
                                            # clipBy' (rect wi hi))
                   <> rect wi hi # value mempty # lw 0.02
                         )  # freeze  # frame 0.1

            return $ CWidget (liftM ((,) ([],[])) s) render

        Combobox xs (bs, br) -> do
            let n = length xs
            iss <- replicateM n newId
            ii <- newId

            let -- ff ind _ _ (Just ' ') = br ind
                br' ind = br (ind `mod` n)
                br'' f = liftReadRef bs >>= br' . f 
                ff [] _ (Just '\n') = br'' (+1)
                ff [] _ (Just ' ') = br'' (+1)
                ff [] "BackSpace" _ = br'' (+(-1))
                ff a b c = dkh a b c
                kh = (return (), ff, return (), ii)

                x = maximum [x | txt <- xs, let (x :& _) = fst $ text__ 15 3 txt ]

                render bv is is' = vcat (zipWith3 g [0..] iss xs) # freeze # frame 0.1
                  where
                    g ind i txt = (
                       (if bv == ind then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)] 
                                # lineCap LineCapRound else mempty) # value mempty # translate (r2 (x / 2,0)) # lw 0.15
                      <> te # clipped (rect x y) # value mempty
                      <> rect x y # (if i `elem` is then fc yellow else id)
                             # (if is' == ii then lc yellow . lw focWidth else lc black . lw 0.02)
                             # value_ (br ind) kh i
                            )  # frame 0.02

                     where ((_ :& y), te) = text__ 15 3 txt

            return $ CWidget (liftM ((,) ([kh],[[kh]])) bs) render

        Notebook' br xs -> do
            let (names, wis) = unzip xs
                n = length names
            iss <- replicateM n newId
            ii <- newId
            ir <- lift $ newRef (0 :: Int)

            let br' :: Int -> Modifier m ()
                br' ind = br ind' >> writeRef ir ind' where ind' = ind `mod` n
                br'' f = readRef' ir >>= br' . f 
                ff [] "Left" _ = br'' (+(-1))
                ff [] "Right" _ = br'' (+ 1)
                ff [AltModifier] _ (Just c) | Just i <- ind c = br'' (const i)
                ff a b c = dkh a b c

                fff [AltModifier] _ (Just c) | Just i <- ind c = br'' (const i)
                fff a b c = dkh a b c

                ind c | 0 <= i && i < n = Just i
                      | otherwise = Nothing
                  where i = fromEnum c - fromEnum '1'

            wisv <- mapM (tr sca fff) wis

            wr <- lift $ onChangeSimple (readRef ir) $ \x -> return $ case wisv !! x of
                         CWidget rr render -> do
                           (es, rrv) <- rr
                           return $ (es, UnsafeEqWrap (x, rrv) $ render rrv)

            let kh = (return (), ff, return (), ii)

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
                             # (if bv == ind then value mempty else value_ (br' ind) kh i)
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

            return $ CWidget (liftM2 (\iv ((ls,ls'),vv) -> ((kh:ls,[kh]:ls'), (iv, vv))) (readRef ir) (join wr)) render

        Scale mi ma step (sr,rr) -> do
            i <- newId
            mv <- lift $ newRef Nothing

            let ff [] "Right" _ = modR $ min ma . (+step)
                ff [] "Left" _  = modR $ max mi . (+(-step))
                ff a b c = dkh a b c
                kh = (return (), ff, return (), i)
                modR f = do
                    v <- liftReadRef sr
                    rr $ f v

                adj x = do
                    m <- readRef' mv
                    case m of
                        Just (x_, v) -> rr $ min ma $ max mi $ v + (ma - mi) * (x - x_) / 10
                        Nothing -> return ()

                f (Click (MousePos (x,_) _)) = Just' (liftReadRef sr >>= \v -> writeRef mv $ Just (x, v), Just f', Just kh, i)
                f (MoveTo _) = Just' (return (), Nothing, Nothing, i)
                f _ = mempty

                f' (Release (MousePos (x,_) _)) = adj x >> writeRef mv Nothing >> return False
                f' (MoveTo  (MousePos (x,_) _)) = adj x >> return True
                f' _ = return True

                render r is is' =
                    (  circle 0.38 # value f
                                   # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                   # lw 0.02 -- (if is' == i then lc yellow . lw focWidth else lc black . lw 0.02)
                                   # translate (r2 (10 * ((r - mi) / (ma - mi)) - 5, 0))
                    <> (  fromVertices [p2 (-5, 0), p2 (5,0)] # lineCap LineCapRound # lw 0.05
                       <> roundedRect 11 1 0.5 # lw 0 # fc (if is' == i then yellow else sRGB 0.95 0.95 0.95)
                       )  # value mempty
                    ) # freeze # frame 0.1

            return $ CWidget (liftM ((,) ([kh],[[kh]])) sr) render


--------------------

data UnsafeEqWrap b = forall a . Eq a => UnsafeEqWrap a b
instance Eq (UnsafeEqWrap b) where
    UnsafeEqWrap a _ == UnsafeEqWrap b _ = a == unsafeCoerce b


