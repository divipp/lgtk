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
    , newRef    -- newRef
    , extendRef    -- extendWith

    -- ** Substate formation
    , lensMap
    , toEqRef   -- toEqRef

    -- ** Triggers
    , onChange
    , onChangeEq
    , onChangeEq_
    , onChangeMemo

    -- ** Other
    , memoise  -- memoise

    -- ** Reference writing
    , writeRef  -- writeRef
    , adjust    -- modify

    -- ** Reference reading
    , value   -- value
    , liftRefReader -- liftRefReader

    -- * Types
    , Widget
    , RefReader
    , RefCreator
    , RefWriter ()
    , Modifier
    , Ref
    , EqRef

    -- ** Other types
    , RefSimple
--    , RefReaderSimple
--    , EqRefClass
    , RefClass

    ) where

--import Data.String
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Semigroup
import Control.Applicative
--import Control.Monad
import Control.Lens

import Data.LensRef (RefClass (RefReaderSimple), RefSimple, EqRefClass, hasEffect, fromEqRef, RefReaderOf, MonadRefReader, BaseRef, RefWriterOf, toEqRef)
import qualified Data.LensRef as Ref
import qualified Data.LensRef.Default as Ref
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

value
    :: ( MonadRefReader m
       , RefClass r
       , RefReaderOf m ~ RefReaderSimple r
       , BaseRef m ~ BaseRef (RefWriterOf m)
       )
    => RefSimple r a -> m a
value = Ref.readRef

liftRefReader
    :: ( MonadRefReader m
       , BaseRef m ~ BaseRef (RefWriterOf m)
       )
    => RefReaderOf m a -> m a
liftRefReader = Ref.liftRefReader

unitRef :: RefClass r => RefSimple r ()
unitRef = Ref.unitRef

infixr 8 `lensMap`

lensMap :: RefClass r => Lens' a b -> RefSimple r a -> RefSimple r b
lensMap = Ref.lensMap

writeRef :: (RefClass r, RefReaderSimple r ~ RefReader) => RefSimple r a -> a -> RefWriter ()
writeRef = Ref.writeRef

adjust :: (RefClass r, RefReaderSimple r ~ RefReader) => RefSimple r a -> (a -> a) -> RefWriter ()
adjust = Ref.modRef

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
{-
extendStateEq :: Eq a => a -> RefCreator (Ref a)
extendStateEq = Ref.newEqRef
-}
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



{- |
Widget descriptions.
-}
type Widget = Widget.Widget RefCreator

-- | Substate of the program state.
type Ref a = Ref.RefOf RefCreator a

-- | Substate of the program state equipped with an @Eq@ instance.
type EqRef a = Ref.EqRefOf RefCreator a

-- | An action which modifies the program state.
type RefWriter = Ref.RefWriterT IO

-- | Program state modifier monad.
type Modifier = RefWriter ()

-- | RefReader of the program state. @RefReader@ is a @Monad@.
type RefReader = Ref.RefReaderT Base

-- | Substate creation monad. Effects can also be emitted in it.
type RefCreator = Eff.RefCreatorPost Base




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
button r fm = primButton r (fmap isJust fm) Nothing (liftRefReader fm >>= maybe (pure ()) id)

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
entry :: (RefClass r, RefReaderSimple r ~ RefReader)  => RefSimple r String -> Widget
entry r = pure $ Entry (const True) ((value r), writeRef r)

-- | Text entry with automatic show-read conversion.
entryShow :: forall a r . (Show a, Read a, RefClass r, RefReaderSimple r ~ RefReader) => RefSimple r a -> Widget
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
    -> KeyboardHandler RefWriter -- ^ keyboard event handler
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


