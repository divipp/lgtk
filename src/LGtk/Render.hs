{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Typeable
import Data.Maybe
import Diagrams.Prelude
--import Diagrams.BoundingBox
--import Diagrams.Backend.Cairo.Text

--import Graphics.SVGFonts
import Data.Colour.SRGB
import Unsafe.Coerce

import LensRef
import LGtk.Key
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

clipBy' p d = fmap unX (fmap X d # clipBy p # withEnvelope p  <> fmap flipp (stroke p # lwL 0 # value Cancel))
  where
    flipp Cancel = Z
    flipp Z = Cancel

    unX (X a) = a
    unX _ = mempty

--------------------------------------------------------- Identifiers

newtype Id = Id [Int]
    deriving (Eq, Ord, Show, Typeable)

type WithId = StateT Id

firstId :: Id
firstId = Id []

newId :: (Monad m, Applicative m) => WithId m Id
newId = do
  x@(Id (i:is)) <- get
  put $ Id $ i+1:is
  pure x

newIds :: Monad m => Id -> WithId m a -> m a
newIds (Id i) = flip evalStateT $ Id (0:i)

instance IsName Id where

---------------------------------------------------------

-- how to handle keyboard events
type KeyHandler m = ModifiedKey -> RefWriter m Bool

-- focus enter action; keyboard event handler; focus leave action; id of the keyboard-focused widget
type KeyFocusHandler m = (RefWriter m (), KeyHandler m, RefWriter m (), Id)

type CapturedEventHandler a m = (MouseEvent a, Dia a) -> RefWriter m Bool

-- for each mouse event:
--  - what to do
--  - the new keyboard focus handler
--  - the id of the mouse-focused widget
type EventHandler a m = (MouseEvent a, Dia a) -> Maybe' (RefWriter m (), Maybe (CapturedEventHandler a m), Maybe (KeyFocusHandler m), Id)

-- compiled widget
data CWidget m
    = forall a x . (Eq x, Monoid a, Semigroup a)
    => CWidget (RefReader m (([KeyFocusHandler m], [[KeyFocusHandler m]]), x)) (a -> EventHandler () m) (x -> [Id] -> Id -> Dia a)

------------------

value_ :: (RefContext m) => RefWriter m () -> KeyFocusHandler m -> Id -> Dia Any -> Dia (EventHandler () m)
value_ a c i = value $ valueFun a c i

valueFun a c i = f where
    f (Click _, _di) = Just' (a, Nothing, Just c, i)
    f (MoveTo _, _di) = Just' (pure (), Nothing, Nothing, i)
    f _ = mempty

-----------------

[] !!! _ = Nothing
_ !!! n | n < 0 = Nothing
(x:_) !!! 0 = Just x
(_:xs) !!! n = xs !!! (n-1 :: Int)

[x] !!!! _ = x
(x:_) !!!! n | n <= 0 = x
(_:xs) !!!! n = xs !!!! (n-1 :: Int)
[] !!!! _ = error "(!!!!) was applied to an empty list."

inCanvas :: forall m . (RefContext m, MonadFix m) => Int -> Int -> Double -> Widget m -> Widget m
inCanvas width height scale w = mdo

    let i = firstId

    foc <- newRef df
    rememberfoc <- newRef df

    -- captured event handler
    capt <- newRef Nothing

    -- mouse-focused widget id
    hi <- newRef [i]

    let df = (pure (), dkh, pure (), i)

        calcPos i jss = listToMaybe [(a,b) | (a,js) <- zip [0..] jss, (b, j) <- zip [0..] js, i == j]

        changeFoc f = do
            (_, _, _, j) <- readerToWriter $ readRef foc
            (_, xss_) <- readerToWriter bb
            let xss = filter (not . null) xss_
            if null xss
              then pure False
              else do
                let mp = calcPos j $ map (map (\(_,_,_,i)->i)) xss
                    ((a,_), (a',b')) = maybe ((0,0), (0,0)) (\x -> (x, f x)) mp
                if a == a'
                  then maybe (pure False) ((>> pure True) . h2) $ (xss !! a') !!! b'
                  else maybe (pure False) ((>> pure True) . h2) $ fmap (!!!! b') (xss !!! a')

        dkh (SimpleKey Key'Up)     = changeFoc $ \(a,b) -> (a-1,b)
        dkh (SimpleKey Key'Down)   = changeFoc $ \(a,b) -> (a+1,b)
        dkh (SimpleKey Key'Left)   = changeFoc $ \(a,b) -> (a,b-1)
        dkh (SimpleKey Key'Right)  = changeFoc $ \(a,b) -> (a,b+1)
        dkh (SimpleKey Key'Tab)    = moveFoc True
        dkh (ControlKey Key'Tab)   = moveFoc False
        dkh _ = pure False

        h2 m@(a,_,_,i) = do
            i' <- readerToWriter $ readRef $ _4 `lensMap` foc
            when (i /= i') $ do
                join $ readerToWriter $ readRef $ _3 `lensMap` foc
                a
                writeRef foc m

        moveFoc f = do
            (_, _, _, j) <- readerToWriter $ readRef foc
            (xs, _) <- readerToWriter bb
            h2 $ maybe (head xs) snd $ find (\((_,_,_,x),_) -> x == j) $ pairs $ (if f then id else reverse) (xs ++ xs)
            pure $ if f then j /= last xs ^. _4 else j /= head xs ^. _4

    -- compiled widget
    bhr <- newIds firstId $ tr (fromIntegral width / scale) dkh w

    let bb = case bhr of
           CWidget b _ _ -> fmap fst b

    case bhr of
       CWidget b hr render -> do

        let handle_ Nothing' = writeRef hi [i] -- pure ()
            handle_ (Just' (a, cap, bb, i)) = do
                a
                maybe (pure ()) (writeRef capt . Just) cap
                maybe (pure ()) h2 bb
                writeRef hi [i]
                pure ()

            hr_ (Just' x) = hr x
            hr_ Nothing' = valueFun (pure ()) df i

            handle f x = do
                m <- readerToWriter $ readRef capt
                case m of
                    Nothing -> handle_ $ hr_ f x
                    Just f -> do
                        b <- f x
                        when (not b) $ writeRef capt Nothing

            handleEvent (Release (MousePos p f), di) = handle f (Release $ MousePos p (), di # clearValue # value ())
            handleEvent (Click   (MousePos p f), di) = handle f (Click   $ MousePos p (), di # clearValue # value ())
            handleEvent (MoveTo  (MousePos p f), di) = handle f (MoveTo  $ MousePos p (), di # clearValue # value ())
            handleEvent (GetFocus, _di) = readerToWriter (readRef rememberfoc) >>= h2
            handleEvent (LostFocus, _di) = readerToWriter (readRef foc) >>= writeRef rememberfoc >> h2 df
            handleEvent _ = pure ()

            handleKeys key = do
                (_,f,_,_) <- readerToWriter $ readRef foc
                f key

        pure $ Canvas width height scale handleEvent (Just handleKeys) (liftA3 (,,) (readRef hi) (readRef $ _4 `lensMap` foc) $ fmap snd b) $
            \(is, is', x) ->
                 fmap Just' (render x is is' # alignT # alignL # translate (r2 (-scale/2,scale/2* fromIntegral height / fromIntegral width)))
              <> fmap (const Nothing') (rect scale (scale / fromIntegral width * fromIntegral height) # value ())

focWidth = 0.1

{-
text_ s = (coords $ boxExtents (boundingBox t) + r2 (0.2, 0.2) , t) where
    t = textSVG s 1.8 # stroke # fc black
-}
text__ :: Double -> Double -> String -> ((Double :& Double), Dia Any)

text__ ma mi s = (x :& y, text s # scale 0.6 # clipped (rect x y))
  where
    x = max mi (min ma $ fromIntegral (length s) * 2/3)
    y = 1
{-
text__ ma mi s = ((x' :& y'), -- rect x' y' # fc red) --
                              d' # scale (1/y) # centerXY # clipped (rect x' y')  -- <> rect x' y' # lwL 0 # fc white
                            )
  where
    d = textLineBounded (fontSize (Local 1.1) mempty) s
    bb = boundingBox d
    Just (p1, p2) = getCorners bb
    d' = d <> circle 0.1 # moveTo p1 <> circle 0.1 # moveTo p2

    (x :& y) = coords $ boxExtents bb
    x' = max mi (min ma x) / y
    y' = 1
-}

defcolor = sRGB 0.95 0.95 0.95

tr  :: forall m . RefContext m
    => Double
    -> KeyHandler m
    -> Widget m
    -> WithId (RefCreator m) (CWidget m)
tr sca dkh w = do
    w' <- lift w
    case w' of
        Label r -> do
            let render bv _ _ = ((rect x y # lwL 0 <> te) # clipped (rect x y)) # value mempty
                     where ((x :& y), te) = text__ 25 5 bv
            pure $ CWidget (fmap ((,) ([], [])) r) id render

        Button r sens col a -> do
            i <- newId

            let ff (CharKey ' ') = a () >> pure True
                ff (CharKey '\n') = a () >> pure True
                ff k = dkh k
                kh = (pure (), ff, pure (), i)

                col' = maybe (pure black) id col

                render (bv, se, color) is is' =
                     (te # fc (if se then color else gray)
                  <> roundedRect x y 0.3 # fc (if i `elem` is && se then yellow else defcolor)
                         # (if is' == i && se then lc yellow . lwL focWidth else lc black . lwL 0.02)
                     )
                        # (if se then value_ (a ()) kh i else value mempty)
                        # clipBy' (rect (x+0.1) (y+0.1)) # frame 0.1
                   where ((x :& y), te) = text__ 15 3 bv
            pure $ CWidget (liftA3 (\r se c -> (([kh | se], [[kh] | se]), (r,se,c))) r sens col') id render

        Entry isOk (rs, rr) -> do
            i <- newId
            j1 <- lift $ newRef False
            j2 <- lift $ onChangeEq_ rs $ \s -> pure (reverse s, "")

            let
                update s = writeRef j2 (reverse s, "")

            let f (SimpleKey Key'Tab) _ = Nothing
                f (CharKey c) (a,b) = Just (c:a,b)
                f (SimpleKey Key'Backspace) (_:a,b) = Just (a,b)
                f (SimpleKey Key'Delete) (a,_:b) = Just (a,b)
                f (SimpleKey Key'Left) (c:a,b) = Just (a,c:b)
                f (SimpleKey Key'Right) (a,c:b) = Just (c:a,b)
                f _ _ = Nothing

                commit = do
                    ab <- readerToWriter $ readRef j2
                    let s = value' ab
                    old <- readerToWriter rs
                    when (s /= old && isOk s) $ do
                        rr s
                        new <- readerToWriter rs
                        update new

                ff (CharKey '\n') = commit >> pure True
                ff key = do
                    x <- readerToWriter $ readRef j2
                    case f key x of
                        Just x -> writeRef j2 x >> pure True
                        _ -> dkh key

                value' (a,b) = reverse a ++ b

                text' (False,ab) = value' ab
                text' (True,(a,b)) = reverse a ++ "|" ++ b
                isOk' (_,ab) = isOk $ value' ab

                kh = (fin, ff, fout, i)
                  where
                    fin = writeRef j1 True
                    fout = do
                        commit
                        writeRef j1 False

                render (orig,bv) is is' =
                  (  te # clipped (rect x y) # value mempty
                  <> rect x y # (if isOk' bv then (if i `elem` is then fc yellow else (if orig /= value' (snd bv) then fc defcolor else id)) else fc red)
                         # (if is' == i then lc yellow . lwL focWidth else lc black . lwL 0.02)
                         # value_ (pure ()) kh i
                  ) # frame 0.1
                   where ((x :& y), te) = text__ 7 5 $ text' bv

            pure $ CWidget (fmap ((,) ([kh],[[kh]])) (liftA2 (,) rs (liftA2 (,) (readRef j1) (readRef j2)))) id render

        Checkbox (bs, br) -> do
            i <- newId

            let ff (CharKey ' ') = readerToWriter bs >>= br . not >> pure True
                ff k = dkh k
                kh = (pure (), ff, pure (), i)

                render bv is is' =
                    (
                       (if bv then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)]
                                # lineCap LineCapRound else mempty) # value mempty # lwL 0.15
                    <> rect 1 1 # value_ (br (not bv)) kh i
                                # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                # (if is' == i then lc yellow . lwL focWidth else lc black . lwL 0.02)
                    ) # frame 0.1
            pure $ CWidget (fmap ((,) ([kh],[[kh]])) bs) id render

        Cell r f -> do
            i <- newId
            r' <- lift $ onChangeMemo r $ \x -> do
                     h <- f (newIds i . tr sca dkh) x
                     pure $ do
                       hv <- h
                       pure $ case hv of
                         CWidget rr hr render -> flip fmap rr $
                            \(es, rrv) -> (es, UnsafeEqWrap (x, rrv) $ fmap (fmap (fmap hr)) $ render rrv)
            pure $ CWidget (join_ r') id $ \(UnsafeEqWrap _ d) is is' -> d is is'

        List layout ws -> fmap (foldr conc2 nil) $ mapM (tr sca dkh) ws
          where
--            nil :: MonadRefReader n => CWidget n
            nil = CWidget (pure (([],[]),())) id mempty

            conc2 (CWidget b hr r) (CWidget b' hr' r')
              = CWidget (liftA2 (\((a,a'),b)((c,c'),d)->((a++c,comb layout a' c'),(b,d))) b b') id (\(x,y) -> liftA2 (liftA2 ff) (fmap (fmap (fmap hr)) $ r x) (fmap (fmap (fmap hr')) $ r' y))

            comb Vertical = (++)
            comb Horizontal = fx

            fx (a:as) (b:bs) = (a++b): fx as bs
            fx as bs = as ++ bs

            ff = case layout of
                Horizontal -> \a b -> a # alignT ||| b # alignT
                Vertical -> \a b -> a # alignL === b # alignL

        Canvas w h d r keyh s f -> do

            i <- newId

            let ff key = do
                    b <- fromMaybe (\_ -> pure False) keyh key
                    if b then pure True else dkh key

                tr di p = case lookupName i di of
                            Just subd -> p # translate (p2 (0,0) .-. q) # scale (1/((fromIntegral w / d) / sca))
                                where q = location subd
                            Nothing -> error "Impossible: could not find the frame of a canvas."

--                decomp (x :& y) = (x,y)

                gg (Just' ls) (Click (MousePos p _), di) = Just' (r (Click $ MousePos (tr di p) ls, di # clearValue # value ls) >> pure (), Nothing, Just kh, i)
                gg (Just' ls) (Release (MousePos p _), di) = Just' (r (Release $ MousePos (tr di p) ls, di # clearValue # value ls) >> pure (), Nothing, Nothing, i)
                gg (Just' ls) (MoveTo  (MousePos p _), di) = Just' (r (MoveTo  $ MousePos (tr di p) ls, di # clearValue # value ls) >> pure (), Nothing, Nothing, i)
                gg _ _ = mempty

                wi = fromIntegral w / sca
                hi = fromIntegral h / sca

                kh = (r (GetFocus, undefined) >> pure (), ff, r (LostFocus, undefined) >> pure (), i)

                render bv _is is' = (fmap gg (fmap Just' (f bv) # scale ((fromIntegral w / d) / sca)
                                            # clipBy' (rect wi hi))
                   <> rect wi hi # named i # value mempty # lwL 0.02 # lc (if is' == i then yellow else black)
                         )  # frame 0.1

            pure $ CWidget (fmap ((,) ([kh | isJust keyh],[[kh] | isJust keyh])) s) id render

        Combobox xs (bs, br) -> do
            let n = length xs
            iss <- replicateM n newId
            ii <- newId

            let -- ff ind _ _ (Just ' ') = br ind
                br' ind = br (ind `mod` n)
                br'' f = readerToWriter bs >>= br' . f  >> pure True
                ff (CharKey '\n') = br'' (+1)
                ff (CharKey ' ') = br'' (+1)
                ff (SimpleKey Key'Backspace) = br'' (+(-1))
                ff k = dkh k
                kh = (pure (), ff, pure (), ii)

                x = maximum [x | txt <- xs, let (x :& _) = fst $ text__ 15 3 txt ]

                render bv is is' = vcat (zipWith3 g [0..] iss xs) # frame 0.1
                  where
                    g ind i txt = (
                       (if bv == ind then fromVertices [p2 (-0.3, 0), p2 (-0.1,-0.3), p2 (0.3,0.3)]
                                # lineCap LineCapRound else mempty) # value mempty # translate (r2 (x / 2,0)) # lwL 0.15
                      <> te # clipped (rect x y) # value mempty
                      <> rect x y # (if i `elem` is then fc yellow else id)
                             # (if is' == ii then lc yellow . lwL focWidth else lc black . lwL 0.02)
                             # value_ (br ind) kh i
                            )  # frame 0.02

                     where ((_ :& y), te) = text__ 15 3 txt

            pure $ CWidget (fmap ((,) ([kh],[[kh]])) bs) id render

        Notebook' br xs -> do
            let (names, wis) = unzip xs
                n = length names
            iss <- replicateM n newId
            ii <- newId
            ir <- lift $ newRef (0 :: Int)

            let br' :: Int -> RefWriter m ()
                br' ind = br ind' >> writeRef ir ind' where ind' = ind `mod` n
                br'' f = readerToWriter (readRef ir) >>= br' . f  >> pure True
                ff (SimpleKey Key'Left) = br'' (+(-1))
                ff (SimpleKey Key'Right) = br'' (+ 1)
                ff (AltKey (Key'Char c)) | Just i <- ind c = br'' (const i)
                ff k = dkh k

                ind c | 0 <= i && i < n = Just i
                      | otherwise = Nothing
                  where i = fromEnum c - fromEnum '1'

            wisv <- mapM (tr sca dkh) wis

            wr <- lift $ onChangeEq (readRef ir) $ \x -> case wisv !! x of
                         CWidget rr hr render -> pure $ flip fmap rr $
                           \(es, rrv) -> (es, UnsafeEqWrap (x, rrv) $ fmap (fmap (fmap hr)) $ render rrv)

            let kh = (pure (), ff, pure (), ii)

                render (bv, UnsafeEqWrap _ d) is is' =
                    vcat
                        [ strutY 0.1
                        , alignL $ line 0.2
                           ||| hcat (intersperse (line 0.1) $ zipWith3 g [0..] iss names)
                           ||| line 100
                        , strutY 0.2
                        , alignL $ d is is'
                        ]
                  where
                    line dx = fromOffsets [r2 (dx,0)] # strokeLine # translate (r2 (0,-0.5)) # value mempty # lcw

                    lcw = if is' == ii then lc yellow . lwL focWidth else lc black . lwL 0.02

                    g ind i txt =
                         te # clipped (rect x y) # value mempty
                             # fc black
                      <> bez # value mempty
                             # lcw
                      <> (if bv == ind then mempty else line x # translate (r2 (-x/2, 0))) # lcw
                      <> bez' # closeLine # strokeLoop  # translate (r2 (-x/2,-y/2)) # lwL 0
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

            pure $ CWidget (liftA2 (\iv ((ls,ls'),vv) -> ((kh:ls,[kh]:ls'), (iv, vv))) (readRef ir) (join_ wr)) id render

        Scale mi ma step (sr,rr) -> do
            i <- newId
            mv <- lift $ newRef Nothing

            let ff (SimpleKey Key'Right) = modR $ min ma . (+step)
                ff (SimpleKey Key'Left) = modR $ max mi . (+(-step))
                ff k = dkh k
                kh = (pure (), ff, pure (), i)
                modR f = do
                    v <- readerToWriter sr
                    rr $ f v
                    pure True

                adj x = do
                    m <- readerToWriter $ readRef mv
                    case m of
                        Just (x_, v) -> rr $ min ma $ max mi $ v + (ma - mi) * (x - x_) / 10
                        Nothing -> pure ()

                f (Click (MousePos p _), _) = Just' (readerToWriter sr >>= \v -> writeRef mv $ Just (getx p, v), Just f', Just kh, i)
                f (MoveTo _, _) = Just' (pure (), Nothing, Nothing, i)
                f _ = mempty

                f' (Release (MousePos p _), _) = adj (getx p) >> writeRef mv Nothing >> pure False
                f' (MoveTo  (MousePos p _), _) = adj (getx p) >> pure True
                f' _ = pure True

                getx p = case coords p of (x :& _) -> x

                render r is is' =
                    (  circle 0.38 # value f
                                   # fc (if i `elem` is then yellow else sRGB 0.95 0.95 0.95)
                                   # lwL 0.02 -- (if is' == i then lc yellow . lwL focWidth else lc black . lwL 0.02)
                                   # translate (r2 (10 * ((r - mi) / (ma - mi)) - 5, 0))
                    <> (  fromVertices [p2 (-5, 0), p2 (5,0)] # lineCap LineCapRound # lwL 0.05
                       <> roundedRect 11 1 0.5 # lwL 0 # fc (if is' == i then yellow else sRGB 0.95 0.95 0.95)
                       )  # value mempty
                    ) # frame 0.1

            pure $ CWidget (fmap ((,) ([kh],[[kh]])) sr) id render

join_ = join

--------------------

data UnsafeEqWrap b = forall a . Eq a => UnsafeEqWrap a b
instance Eq (UnsafeEqWrap b) where
    UnsafeEqWrap a _ == UnsafeEqWrap b _ = a == unsafeCoerce b
