{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Main LGtk interface.
module LGtk
    (
    -- * Widgets

    -- ** Widget elements
      emptyWidget
    , label
    , button
    , smartButton
    , primButton
    , checkbox
    , combobox
    , entry
    , entryShow
    , hscale

    -- ** Widget composition
    , horizontally
    , vertically
    , notebook
    , cell
    , cellNoMemo

    -- ** Canvas
    , canvas
    , inCanvas

    , Dia
    , module LGtk.Key
    , MouseEvent (..)
    , MousePos (..)
    , Colour
    , sRGB

    -- ** Running a widget
    , runWidget

    -- * Utils
    , undoTr
    , showLens
    , listLens

    -- * I/O
    , getArgs
    , getProgName
    , lookupEnv
    , putStr_
    , getLine_
    , fileRef
    , putStrLn_

    -- ** Timing
    , time
    , atTime
    , asyncWrite
    , UTCTime
    , NominalDiffTime
    , addUTCTime
    , diffUTCTime


    -- * References

    -- ** State extension
    , newRef
    , extendRef

    -- ** Substate formation
    , lensMap
    , joinRef
    , toEqRef

    -- ** Triggers
    , onChange
    , onChangeEq
    , onChangeEq_
    , onChangeMemo

    -- ** Other
    , memoise

    -- ** Reference writing
    , writeRef
    , adjust    -- modRef

    -- ** Reference reading
    , value   -- readRef
    , currentValue

    -- ** Conversion between monads
    , readerToWriter
    , readerToCreator

    -- * Types
    , Widget
    , RefReader
    , RefCreator
    , RefWriter ()
    , Modifier
    , Ref
    , EqRef
    , RefClass
    ) where

--import Data.String
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Semigroup
import Control.Applicative
import Control.Lens

import Data.LensRef (readerToCreator)
import qualified Data.LensRef as Ref
import LGtk.Effects ()
import qualified LGtk.Effects as Eff
import LGtk.Widgets hiding (Widget)
import qualified LGtk.Widgets as Widget
import qualified LGtk.Render as Render
import LGtk.Key

#ifdef __GTK__
import LGtk.Backend.Gtk
#else
import LGtk.Backend.GLFW
#endif

----------------------------

class RefClass r where
    readRef :: r a -> RefReader a
    writeRef :: r a -> a -> RefWriter ()
    lensMap :: Lens' a b -> r a -> r b
    joinRef :: RefReader (r a) -> r a

r `modRef` f = Ref.readerToWriter (readRef r) >>= writeRef r . f

infixr 8 `lensMap`

instance RefClass Ref where
    readRef = Ref.readRef
    writeRef = Ref.writeRef
    lensMap = Ref.lensMap
    joinRef = Ref.joinRef

-- | Substate of the program state equipped with an @Eq@ instance.
data EqRef a = EqRef
    { runEqRef :: Ref a
    , changing :: a -> RefReader Bool
    }

instance RefClass EqRef where
    readRef = Ref.readRef . runEqRef
    writeRef r = Ref.writeRef (runEqRef r)
    lensMap k (EqRef r c) = EqRef
        { runEqRef = Ref.lensMap k r
        , changing = \b -> Ref.readRef r >>= \a -> c $ set k b a
        }
    joinRef m = EqRef
        { runEqRef = Ref.joinRef $ m <&> runEqRef
        , changing = \a -> m >>= \(EqRef _ k) -> k a
        }

hasEffect
    :: EqRef a
    -> (a -> a)
    -> RefReader Bool
hasEffect (EqRef r c) f = readRef r >>= c . f

toEqRef :: (Eq a) => Ref a -> EqRef a
toEqRef r = EqRef r $ \x -> Ref.readRef r <&> (/= x)


value = readRef

currentValue
    :: RefReader a -> RefWriter a
currentValue = Ref.readerToWriter

readerToWriter = currentValue

adjust :: (RefClass r) => r a -> (a -> a) -> RefWriter ()
adjust = modRef

extendRef :: Ref b -> Lens' a b -> a -> RefCreator (Ref a)
extendRef = Ref.extendRef

newRef :: a -> RefCreator (Ref a)
newRef = Ref.newRef

onChange :: RefReader a -> (a -> RefCreator b) -> RefCreator (RefReader b)
onChange = Ref.onChange

onChangeEq :: Eq a => RefReader a -> (a -> RefCreator b) -> RefCreator (RefReader b)
onChangeEq = Ref.onChangeEq

onChangeEq_ :: Eq a => RefReader a -> (a -> RefCreator b) -> RefCreator (Ref b)
onChangeEq_ = Ref.onChangeEq_

onChangeMemo :: Eq a => RefReader a -> (a -> RefCreator (RefCreator b)) -> RefCreator (RefReader b)
onChangeMemo = Ref.onChangeMemo

memoise :: RefCreator a -> RefCreator (RefCreator a)
memoise = Ref.memoRead

getArgs :: RefCreator [String]
getArgs = Eff.getArgs

getProgName :: RefCreator String
getProgName = Eff.getProgName

lookupEnv :: String -> RefCreator (Maybe String)
lookupEnv = Eff.lookupEnv

asyncWrite :: Int -> RefWriter () -> RefCreator ()
asyncWrite = Eff.asyncWrite

atTime :: UTCTime -> RefWriter () -> RefCreator ()
atTime = Eff.atTime

time :: RefCreator UTCTime
time = Eff.time

putStr_ :: String -> RefCreator ()
putStr_ = Eff.putStr_

putStrLn_ :: String -> RefCreator ()
putStrLn_ = Eff.putStrLn_

getLine_ :: (String -> RefWriter ()) -> RefCreator ()
getLine_ = Eff.getLine_

fileRef :: FilePath -> RefCreator (Ref (Maybe String))
fileRef = Eff.fileRef


type RB = Eff.Rt Base

{- |
Widget descriptions.
-}
type Widget = Widget.Widget RB

-- | Substate of the program state.
type Ref = Ref.Ref RB

-- | An action which modifies the program state.
type RefWriter = Ref.RefWriter RB

-- | Program state modifier monad.
type Modifier = RefWriter ()

-- | RefReader of the program state. @RefReader@ is a @Monad@.
type RefReader = Ref.RefReader RB

-- | Substate creation monad. Effects can also be emitted in it.
type RefCreator = Ref.RefCreator RB




{- |
Run a widget description.

The widget is shown in a window and the thread enters into the event cycle.
It leaves the event cycle when the window is closed.
-}
--runWidget = B.runWidget


-- | Vertical composition of widgets.
vertically :: [Widget] -> Widget
vertically = pure . List Vertical

-- | Horizontal composition of widgets.
horizontally :: [Widget] -> Widget
horizontally = pure . List Horizontal

-- | Empty widget.
emptyWidget :: Widget
emptyWidget = horizontally []

-- | Dynamic label.
label :: RefReader String -> Widget
label = pure . Label

-- | Low-level button with changeable background color.
primButton
    :: RefReader String     -- ^ dynamic label of the button
    -> RefReader Bool       -- ^ the button is active when this returns @True@
    -> Maybe (RefReader (Colour Double))      -- ^ dynamic background color
    -> RefWriter ()        -- ^ the action to do when the button is pressed
    -> Widget
primButton r x c y = pure $ Button (r) (x) c (\() -> y)

-- | Button.
button
    :: RefReader String     -- ^ dynamic label of the button
    -> RefReader (Maybe (RefWriter ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget
button r fm = primButton r (fmap isJust fm) Nothing (currentValue fm >>= maybe (pure ()) id)

-- | Button which inactivates itself automatically.
smartButton
    :: RefReader String     -- ^ dynamic label of the button
    -> EqRef a              -- ^ underlying reference
    -> (a -> a)   -- ^ The button is active when this function is not identity on value of the reference. When the button is pressed, the value of the reference is modified with this function.
    -> Widget
smartButton s r f
    = primButton s (hasEffect r f) Nothing (adjust r f)

-- | Checkbox.
checkbox :: Ref Bool -> Widget
checkbox r = pure $ Checkbox ((value r), writeRef r)

-- | Combo box.
combobox :: [String] -> Ref Int -> Widget
combobox ss r = pure $ Combobox ss ((value r), writeRef r)

-- | Text entry.
entry :: (RefClass r)  => r String -> Widget
entry r = pure $ Entry (const True) ((value r), writeRef r)

-- | Text entry with automatic show-read conversion.
entryShow :: forall a r . (Show a, Read a, RefClass r) => r a -> Widget
entryShow r_ = pure $ Entry isOk ((value r), writeRef r)
  where
    r = showLens `lensMap` r_
    isOk s = case (reads s :: [(a, String)]) of
        ((_,""):_) -> True
        _ -> False

showLens :: (Show a, Read a) => Lens' a String
showLens = lens show $ \def s -> maybe def fst $ listToMaybe $ reads s


{- | Notebook (tabs).

The tabs are created lazily.
-}
notebook :: [(String, Widget)] -> Widget
notebook xs = do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell (fmap (== index) $ value currentPage) $ \b -> case b of
           False -> horizontally []
           True -> w
    pure $ Notebook' (writeRef currentPage) $ zipWith f [0..] xs

{- | Dynamic cell.

The inner widgets are memoised.
-}
cell :: (Eq a) => RefReader a -> (a -> Widget) -> Widget
cell r m = pure $ Cell r $ \mk -> fmap pure . mk . m

{- | Dynamic cell.

The inner widgets are not memoised.
-}
cellNoMemo :: (Eq a) => RefReader a -> (a -> Widget) -> Widget
cellNoMemo r m = pure $ Cell r $ \mk -> pure . mk . m

-- | Reactive diagrams canvas.
canvas
    :: (Eq b, Monoid a, Semigroup a)
    => Int   -- ^ width
    -> Int   -- ^ height
    -> Double  -- ^ scale
    -> ((MouseEvent a, Dia a) -> RefWriter ()) -- ^ mouse event handler
    -> KeyboardHandler RB -- ^ keyboard event handler
    -> RefReader b -- ^ state references
    -> (b -> Dia a) -- ^ diagrams renderer
    -> Widget
canvas w h sc me kh r f = pure $ Canvas w h sc me kh r f

-- | Render the widget in canvas.
inCanvas
    :: Int
    -> Int
    -> Double
    -> Widget
    -> Widget 
inCanvas = Render.inCanvas

-- | Horizontal scale.
hscale
    :: Double   -- ^ min
    -> Double   -- ^ max
    -> Double   -- ^ step
    -> Ref Double
    -> Widget
hscale a b c r = pure $ Scale a b c (value r, writeRef r)

listLens :: Lens' (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set (_, x) [] = (False, x)
    set _ (l: r) = (True, (l, r))


-- | Undo-redo state transformation.
undoTr
    :: (a -> a -> Bool)     -- ^ equality on state
    -> Ref a             -- ^ reference of state
    -> RefCreator
           ( RefReader (Maybe (RefWriter ()))
           , RefReader (Maybe (RefWriter ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extendRef r (undoLens eq) ([], [])
    let try f = fmap (fmap (writeRef ku) . f) $ value ku
    pure (try undo, try redo)
  where
    undo (x: xs@(_:_), ys) = Just (xs, x: ys)
    undo _ = Nothing

    redo (xs, y: ys) = Just (y: xs, ys)
    redo _ = Nothing

undoLens :: (a -> a -> Bool) -> Lens' ([a],[a]) a
undoLens eq = lens get set where
    get = head . fst
    set (x' : xs, ys) x | eq x x' = (x: xs, ys)
    set (xs, _) x = (x : xs, [])
