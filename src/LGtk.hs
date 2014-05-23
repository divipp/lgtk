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
      module Data.LensRef

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
    , cell_

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

import Data.LensRef
import LGtk.Effects
import LGtk.Widgets
import LGtk.Render
import LGtk.Key

#ifdef __GTK__
import LGtk.Backend.Gtk
#else
import LGtk.Backend.GLFW
#endif


{- |
Gtk widget descriptions.
Construction of a @(w :: forall m . EffIORef m => Widget m)@ value is side-effect free,
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
--runWidget :: (forall m . EffIORef m => Widget m) -> IO ()
--runWidget = Gtk.runWidget
{-
instance MonadRefState m => IsString (RefStateReader m String) where
    fromString = pure
-}

-- | Vertical composition of widgets.
vcat :: (Monad m, Applicative m) => [Widget m] -> Widget m
vcat = pure . List Vertical

-- | Horizontal composition of widgets.
hcat :: (Monad m, Applicative m) => [Widget m] -> Widget m
hcat = pure . List Horizontal

-- | Empty widget.
empty :: (Monad m, Applicative m) => Widget m
empty = hcat []

-- | Dynamic label.
label :: MonadRegister m => RefReader m String -> Widget m
label = pure . Label

-- | Low-level button with changeable background color.
button__
    :: MonadRegister m
    => RefReader m String     -- ^ dynamic label of the button
    -> RefReader m Bool       -- ^ the button is active when this returns @True@
    -> RefReader m (Colour Double)      -- ^ dynamic background color
    -> RefWriter m ()        -- ^ the action to do when the button is pressed
    -> Widget m
button__ r x c y = pure $ Button (r) (x) (Just c) (\() -> y)

-- | Low-level button.
button_
    :: MonadRegister m
    => RefReader m String     -- ^ dynamic label of the button
    -> RefReader m Bool       -- ^ the button is active when this returns @True@
    -> RefWriter m ()        -- ^ the action to do when the button is pressed
    -> Widget m
button_ r x y = pure $ Button (r) (x) Nothing (\() -> y)

-- | Button
button
    :: MonadRegister m
    => RefReader m String     -- ^ dynamic label of the button
    -> RefReader m (Maybe (RefWriter m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget m
button r fm = button_ r (fmap isJust fm) (liftRefReader fm >>= maybe (pure ()) id)

-- | Button which inactivates itself automatically.
smartButton
    :: (MonadRegister m, EqRefClass r, RefReaderSimple r ~ RefReader m) 
    => RefReader m String     -- ^ dynamic label of the button
    -> RefSimple r a              -- ^ underlying reference
    -> (a -> a)   -- ^ The button is active when this function is not identity on value of the reference. When the button is pressed, the value of the reference is modified with this function.
    -> Widget m
smartButton s r f
    = button_ s (hasEffect r f) (modRef r f)

-- | Checkbox.
checkbox :: MonadRegister m => Ref m Bool -> Widget m
checkbox r = pure $ Checkbox ((readRef r), writeRef r)

-- | Combo box.
combobox :: MonadRegister m => [String] -> Ref m Int -> Widget m
combobox ss r = pure $ Combobox ss ((readRef r), writeRef r)

-- | Text entry.
entry :: (MonadRegister m, RefClass r, RefReaderSimple r ~ RefReader m)  => RefSimple r String -> Widget m
entry r = pure $ Entry (const True) ((readRef r), writeRef r)

-- | Text entry with automatic show-read conversion.
entryShow :: forall m a r . (MonadRegister m, Show a, Read a, RefClass r, RefReaderSimple r ~ RefReader m) => RefSimple r a -> Widget m
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
notebook :: MonadRegister m => [(String, Widget m)] -> Widget m
notebook xs = do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell (fmap (== index) $ readRef currentPage) $ \b -> case b of
           False -> hcat []
           True -> w
    pure $ Notebook' (writeRef currentPage) $ zipWith f [0..] xs

{- | Dynamic cell.

The monadic action for inner widget creation is memoised in the first monad layer.
-}
cell_ :: (MonadRegister m, Eq a) => RefReader m a -> (forall x . (Widget m -> m x) -> a -> m (m x)) -> Widget m
cell_ r f = pure $ Cell r f

{- | Dynamic cell.

The inner widgets are memoised.
-}
cell :: (MonadRegister m, Eq a) => RefReader m a -> (a -> Widget m) -> Widget m
cell r m = cell_ r $ \mk -> fmap pure . mk . m

{- | Dynamic cell.

The inner widgets are not memoised.
-}
cellNoMemo :: (MonadRegister m, Eq a) => RefReader m a -> (a -> Widget m) -> Widget m
cellNoMemo r m = cell_ r $ \mk -> pure . mk . m

-- | Diagrams canvas.
canvas
    :: (MonadRegister m, Eq b, Monoid a, Semigroup a)
    => Int   -- ^ width
    -> Int   -- ^ height
    -> Double  -- ^ scale
    -> ((MouseEvent a, Dia a) -> RefWriter m ()) -- ^ mouse event handler
    -> KeyboardHandler (RefWriter m) -- ^ keyboard event handler
    -> RefReader m b -- ^ state references
    -> (b -> Dia a) -- ^ diagrams renderer
    -> Widget m
canvas w h sc me kh r f = pure $ Canvas w h sc me kh r f

hscale
    :: (MonadRegister m)
    => Double   -- ^ min
    -> Double   -- ^ max
    -> Double   -- ^ step
    -> Ref m Double
    -> Widget m
hscale a b c r = pure $ Scale a b c (readRef r, writeRef r)

listLens :: Lens' (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set (_, x) [] = (False, x)
    set _ (l: r) = (True, (l, r))


-- | Undo-redo state transformation.
undoTr
    :: MonadRegister m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a             -- ^ reference of state
    ->   m ( RefReader m (Maybe (RefWriter m ()))
           , RefReader m (Maybe (RefWriter m ()))
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


