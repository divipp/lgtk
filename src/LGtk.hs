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
    -- * References

    -- ** References
      unitRef
    , lensMap
            -- TODO: elim these?
            , RefReaderSimple, RefClass --RefClass (..)
            , RefSimple
    , MonadRefReader (..)
    , MonadRefWriter (..)
    , Ref
    , RefReaderOf
    , RefWriterOf

    -- ** Reference creation
    , MonadRefCreator (..)
    , RefCreator
    , RefReader
    , RefWriter

    -- ** Other
    , MonadMemo (..)

    , EqRefClass        --EqRefClass (..)
            , hasEffect
--    , EqRefSimple
    , EqRef
    , toEqRef
    , fromEqRef
    , newEqRef

    -- * GUI

    -- ** Running a widget
    , Widget
    , runWidget

    -- ** GUI elements
    , empty
    , vcat
    , hcat
    , label
    , button
    , smartButton
    , checkbox
    , combobox
    , entry
    , entryShow
    , hscale
    , cell
    , cellNoMemo
    , notebook

    -- ** Other elements
    , button_
    , button__

    -- ** Diagrams canvas
    , canvas
    , inCanvas

    -- * Aux types
    , Dia
    , MouseEvent (..)
    , MousePos (..)
    , Colour
    , sRGB
    , module LGtk.Key

    -- * I/O
    , getArgs
    , getProgName
    , lookupEnv

    , EffIORef
    , asyncWrite
    , putStr_
    , getLine_
    , fileRef
    , putStrLn_

    -- * Utils
    , undoTr
    , showLens
    , listLens

    ) where

--import Data.String
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Control.Applicative hiding (empty)
--import Control.Monad
import Control.Lens

import Data.LensRef hiding (Ref, EqRef)
--import Data.LensRef.Default
import LGtk.Effects
import LGtk.Widgets hiding (Widget)
import LGtk.Render
import LGtk.Key

#ifdef __GTK__
import LGtk.Backend.Gtk
#else
import LGtk.Backend.GLFW
#endif

----------------------------

{- |
Gtk widget descriptions.
Construction of a @(w :: forall m . EffIORef m => Widget)@ value is side-effect free,
side-effects happen at running @('runWidget' w)@.

@Widget@ should be abstract data type, but it is also safe to keep it as a type synonym because
the operations of the revealed implementation are hidden.
-}
--runWidgetGLFW = GLFW.runWidget

{- |
Run a Gtk widget description.

The widget is shown in a window and the thread enters into the Gtk event cycle.
It leaves the event cycle when the window is closed.
-}
--runWidget :: (forall m . EffIORef m => Widget) -> IO ()
--runWidget = Gtk.runWidget
{-
instance MonadRefState m => IsString (RefStateReader m String) where
    fromString = pure
-}

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
label :: RefReader String -> Widget
label = pure . Label

-- | Low-level button with changeable background color.
button__
    :: RefReader String     -- ^ dynamic label of the button
    -> RefReader Bool       -- ^ the button is active when this returns @True@
    -> RefReader (Colour Double)      -- ^ dynamic background color
    -> RefWriter ()        -- ^ the action to do when the button is pressed
    -> Widget
button__ r x c y = pure $ Button (r) (x) (Just c) (\() -> y)

-- | Low-level button.
button_
    :: RefReader String     -- ^ dynamic label of the button
    -> RefReader Bool       -- ^ the button is active when this returns @True@
    -> RefWriter ()        -- ^ the action to do when the button is pressed
    -> Widget
button_ r x y = pure $ Button (r) (x) Nothing (\() -> y)

-- | Button
button
    :: RefReader String     -- ^ dynamic label of the button
    -> RefReader (Maybe (RefWriter ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget
button r fm = button_ r (fmap isJust fm) (liftRefReader fm >>= maybe (pure ()) id)

-- | Button which inactivates itself automatically.
smartButton
    :: (EqRefClass r, RefReaderSimple r ~ RefReader) 
    => RefReader String     -- ^ dynamic label of the button
    -> RefSimple r a              -- ^ underlying reference
    -> (a -> a)   -- ^ The button is active when this function is not identity on value of the reference. When the button is pressed, the value of the reference is modified with this function.
    -> Widget
smartButton s r f
    = button_ s (hasEffect r f) (modRef r f)

-- | Checkbox.
checkbox :: Ref Bool -> Widget
checkbox r = pure $ Checkbox ((readRef r), writeRef r)

-- | Combo box.
combobox :: [String] -> Ref Int -> Widget
combobox ss r = pure $ Combobox ss ((readRef r), writeRef r)

-- | Text entry.
entry :: (RefClass r, RefReaderSimple r ~ RefReader)  => RefSimple r String -> Widget
entry r = pure $ Entry (const True) ((readRef r), writeRef r)

-- | Text entry with automatic show-read conversion.
entryShow :: forall a r . (Show a, Read a, RefClass r, RefReaderSimple r ~ RefReader) => RefSimple r a -> Widget
entryShow r_ = pure $ Entry isOk ((readRef r), writeRef r)
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
    let f index (title, w) = (,) title $ cell (fmap (== index) $ readRef currentPage) $ \b -> case b of
           False -> hcat []
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

-- | Diagrams canvas.
canvas
    :: (Eq b, Monoid a, Semigroup a)
    => Int   -- ^ width
    -> Int   -- ^ height
    -> Double  -- ^ scale
    -> ((MouseEvent a, Dia a) -> RefWriter ()) -- ^ mouse event handler
    -> KeyboardHandler (RefWriter) -- ^ keyboard event handler
    -> RefReader b -- ^ state references
    -> (b -> Dia a) -- ^ diagrams renderer
    -> Widget
canvas w h sc me kh r f = pure $ Canvas w h sc me kh r f

hscale
    :: Double   -- ^ min
    -> Double   -- ^ max
    -> Double   -- ^ step
    -> Ref Double
    -> Widget
hscale a b c r = pure $ Scale a b c (readRef r, writeRef r)

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
    ku <- extRef r (undoLens eq) ([], [])
    let try f = fmap (fmap (writeRef ku) . f) $ readRef ku
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


