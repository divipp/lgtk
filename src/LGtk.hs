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

    -- ** Widget creation
      empty
    , vcat
    , hcat
    , label
    , button
    , smartButton
    , primButton
    , checkbox
    , combobox
    , entry
    , entryShow
    , hscale
    , cell
    , cellNoMemo
    , notebook
    , canvas
    , inCanvas

    -- ** Types
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
    , asyncWrite
    , putStr_
    , getLine_
    , fileRef
    , putStrLn_

    -- * References

    -- ** State extension
    , extendState    -- extendState
    , extendStateWith    -- extendWith

    -- ** Substate formation
    , lensMap
    , withEq   -- withEq

    -- ** Triggers
    , onChange
    , onChangeEq
    , onChangeEq_
    , onChangeMemo

    -- ** Other
    , memoise  -- memoise

    -- ** Reference writing
    , write  -- write
    , adjust    -- modify

    -- ** Reference reading
    , value   -- value
    , liftView -- liftView

    -- * Types
    , Widget
    , View
    , Create
    , Modify
    , Modifier
    , SubState
    , SubStateEq
    , RefSimple
--    , RefReaderSimple
--    , EqRefClass
    , RefClass

    ) where

--import Data.String
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Control.Applicative hiding (empty)
--import Control.Monad
import Control.Lens hiding (value)

import Data.LensRef (RefClass (RefReaderSimple), RefSimple, EqRefClass, hasEffect, fromEqRef, RefReaderOf, MonadRefReader, BaseRef, RefWriterOf, toEqRef)
import qualified Data.LensRef as Ref
import LGtk.Effects ()
import qualified LGtk.Effects as Eff
import LGtk.Widgets hiding (Widget)
import qualified LGtk.Render as Render
import LGtk.Key

#ifdef __GTK__
import qualified LGtk.Backend.Gtk as B
#else
import qualified LGtk.Backend.GLFW as B
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

liftView
    :: ( MonadRefReader m
       , BaseRef m ~ BaseRef (RefWriterOf m)
       )
    => RefReaderOf m a -> m a
liftView = Ref.liftRefReader

unitRef :: RefClass r => RefSimple r ()
unitRef = Ref.unitRef

withEq :: Eq a => SubState a -> SubStateEq a
withEq = Ref.toEqRef

infixr 8 `lensMap`

lensMap :: RefClass r => Lens' a b -> RefSimple r a -> RefSimple r b
lensMap = Ref.lensMap

write :: (RefClass r, RefReaderSimple r ~ View) => RefSimple r a -> a -> Modify
write = Ref.writeRef

adjust :: (RefClass r, RefReaderSimple r ~ View) => RefSimple r a -> (a -> a) -> Modify
adjust = Ref.modRef

extendStateWith :: SubState b -> Lens' a b -> a -> Create (SubState a)
extendStateWith = Ref.extRef

extendState :: a -> Create (SubState a)
extendState = Ref.newRef

onChange :: View a -> (a -> Create b) -> Create (View b)
onChange = Ref.onChange

onChangeEq :: Eq a => View a -> (a -> Create b) -> Create (View b)
onChangeEq = Ref.onChangeEq

onChangeEq_ :: Eq a => View a -> (a -> Create b) -> Create (SubState b)
onChangeEq_ = Ref.onChangeEq_

onChangeMemo :: Eq a => View a -> (a -> Create (Create b)) -> Create (View b)
onChangeMemo = Ref.onChangeMemo

memoise :: Create a -> Create (Create a)
memoise = Ref.memoRead
{-
extendStateEq :: Eq a => a -> Create (SubState a)
extendStateEq = Ref.newEqRef
-}
getArgs :: Create [String]
getArgs = Eff.getArgs

getProgName :: Create String
getProgName = Eff.getProgName

lookupEnv :: String -> Create (Maybe String)
lookupEnv = Eff.lookupEnv

asyncWrite :: Int -> Modify -> Create ()
asyncWrite = Eff.asyncWrite

putStr_ :: String -> Create ()
putStr_ = Eff.putStr_

putStrLn_ :: String -> Create ()
putStrLn_ = Eff.putStrLn_

getLine_ :: (String -> Modify) -> Create ()
getLine_ = Eff.getLine_

fileRef :: FilePath -> Create (SubState (Maybe String))
fileRef = Eff.fileRef



{- |
Widget descriptions.
-}
type Widget = B.Widget

-- | Substate of the program state.
type SubState a = B.Ref a

-- | Substate of the program state equipped with an @Eq@ instance.
type SubStateEq a = B.EqRef a

-- | An action which modifies the program state.
type Modify = Modifier ()

-- | Program state modifier monad.
type Modifier = B.RefWriter

-- | View of the program state. @View@ is a @Monad@.
type View = B.RefReader

-- | Substate creation monad. Effects can also be emitted in it.
type Create = B.RefCreator

{- |
Run a Gtk widget description.

The widget is shown in a window and the thread enters into the Gtk event cycle.
It leaves the event cycle when the window is closed.
-}
runWidget = B.runWidget


-- | Vertical composition of widgets.
vcat :: [Widget] -> Widget
vcat = pure . List Vertical

-- | Horizontal composition of widgets.
hcat :: [Widget] -> Widget
hcat = pure . List Horizontal

-- | Empty widget.
empty :: Widget
empty = hcat []

-- | Dynamic label.
label :: View String -> Widget
label = pure . Label

-- | Low-level button with changeable background color.
primButton
    :: View String     -- ^ dynamic label of the button
    -> View Bool       -- ^ the button is active when this returns @True@
    -> Maybe (View (Colour Double))      -- ^ dynamic background color
    -> Modify        -- ^ the action to do when the button is pressed
    -> Widget
primButton r x c y = pure $ Button (r) (x) c (\() -> y)

-- | Button.
button
    :: View String     -- ^ dynamic label of the button
    -> View (Maybe Modify)     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget
button r fm = primButton r (fmap isJust fm) Nothing (liftView fm >>= maybe (pure ()) id)

-- | Button which inactivates itself automatically.
smartButton
    :: View String     -- ^ dynamic label of the button
    -> SubStateEq a              -- ^ underlying reference
    -> (a -> a)   -- ^ The button is active when this function is not identity on value of the reference. When the button is pressed, the value of the reference is modified with this function.
    -> Widget
smartButton s r f
    = primButton s (hasEffect r f) Nothing (adjust r f)

-- | Checkbox.
checkbox :: SubState Bool -> Widget
checkbox r = pure $ Checkbox ((value r), write r)

-- | Combo box.
combobox :: [String] -> SubState Int -> Widget
combobox ss r = pure $ Combobox ss ((value r), write r)

-- | Text entry.
entry :: (RefClass r, RefReaderSimple r ~ View)  => RefSimple r String -> Widget
entry r = pure $ Entry (const True) ((value r), write r)

-- | Text entry with automatic show-read conversion.
entryShow :: forall a r . (Show a, Read a, RefClass r, RefReaderSimple r ~ View) => RefSimple r a -> Widget
entryShow r_ = pure $ Entry isOk ((value r), write r)
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
    currentPage <- extendState 0
    let f index (title, w) = (,) title $ cell (fmap (== index) $ value currentPage) $ \b -> case b of
           False -> hcat []
           True -> w
    pure $ Notebook' (write currentPage) $ zipWith f [0..] xs

{- | Dynamic cell.

The inner widgets are memoised.
-}
cell :: (Eq a) => View a -> (a -> Widget) -> Widget
cell r m = pure $ Cell r $ \mk -> fmap pure . mk . m

{- | Dynamic cell.

The inner widgets are not memoised.
-}
cellNoMemo :: (Eq a) => View a -> (a -> Widget) -> Widget
cellNoMemo r m = pure $ Cell r $ \mk -> pure . mk . m

-- | Diagrams canvas.
canvas
    :: (Eq b, Monoid a, Semigroup a)
    => Int   -- ^ width
    -> Int   -- ^ height
    -> Double  -- ^ scale
    -> ((MouseEvent a, Dia a) -> Modify) -- ^ mouse event handler
    -> KeyboardHandler (Modifier) -- ^ keyboard event handler
    -> View b -- ^ state references
    -> (b -> Dia a) -- ^ diagrams renderer
    -> Widget
canvas w h sc me kh r f = pure $ Canvas w h sc me kh r f

inCanvas
    :: Int
    -> Int
    -> Double
    -> Widget
    -> Widget 
inCanvas = Render.inCanvas

hscale
    :: Double   -- ^ min
    -> Double   -- ^ max
    -> Double   -- ^ step
    -> SubState Double
    -> Widget
hscale a b c r = pure $ Scale a b c (value r, write r)

listLens :: Lens' (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set (_, x) [] = (False, x)
    set _ (l: r) = (True, (l, r))


-- | Undo-redo state transformation.
undoTr
    :: (a -> a -> Bool)     -- ^ equality on state
    -> SubState a             -- ^ reference of state
    -> Create
           ( View (Maybe Modify)
           , View (Maybe Modify)
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extendStateWith r (undoLens eq) ([], [])
    let try f = fmap (fmap (write ku) . f) $ value ku
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


