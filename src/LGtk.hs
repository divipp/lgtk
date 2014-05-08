{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Main LGtk interface.
module LGtk
    (
    -- * References

      Reference, MRef
    , RefReader
    , readRef
    , writeRef, modRef, Modifier
    , lensMap
    , join
    , unitRef

    -- ** Reference creation
    , ExtRef
    , Ref, RefCore
    , extRef
    , newRef
    , ReadRef
    , WriteRef
    , liftReadRef

    -- ** Derived constructs
    , readRef'
    , memoRead
    , undoTr

    , EqReference (..)
    , EqRef
    , eqRef
    , newEqRef
    , toRef

    , CorrRef
    , corrRef
    , fromCorrRef
    , correction

    -- * Dynamic networks
    , EffRef
    , onChange
    , onChangeSimple
    , onChange_
    , iReallyWantToModify

    -- * I/O
    , getArgs
    , getProgName
    , lookupEnv

    , EffIORef
    , asyncWrite
    , putStr_
    , getLine_
    , fileRef

    -- ** Derived constructs
    , putStrLn_

    -- * GUI

    -- ** Running
    , Widget

    -- ** GUI descriptions
    , label
    , checkbox
    , combobox
    , entry
    , vcat
    , hcat
    , button_
    , Colour, sRGB
    , notebook
    , cell_
    , canvas
    , Dia
    , MouseEvent (..)
    , MousePos (..)
    , KeyModifier
    , hscale

    -- ** Rendering into a canvas
    , inCanvas

    -- ** Derived constructs
    , empty
    , entryShow
    , button
    , smartButton
    , cell
    , cellNoMemo

    -- ** Experimental
    , button__

    -- ** Utils
    , showLens
    , listLens

    ) where

--import Data.String
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Control.Monad
import Control.Lens

import Data.LensRef
import LGtk.Effects
import LGtk.Widgets
import LGtk.Render
--import LGtk.Backend.Gtk (runWidget)
--import qualified LGtk.Backend.GLFW as GLFW


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
    fromString = return
-}
-- | Vertical composition of widgets.
vcat :: Monad m => [Widget m] -> Widget m
vcat = return . List Vertical

-- | Horizontal composition of widgets.
hcat :: Monad m => [Widget m] -> Widget m
hcat = return . List Horizontal

-- | Empty widget.
empty :: Monad m => Widget m
empty = hcat []

-- | Dynamic label.
label :: EffRef m => ReadRef m String -> Widget m
label = return . Label

-- | Low-level button with changeable background color.
button__
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m Bool       -- ^ the button is active when this returns @True@
    -> ReadRef m (Colour Double)      -- ^ dynamic background color
    -> Modifier m ()        -- ^ the action to do when the button is pressed
    -> Widget m
button__ r x c y = return $ Button (r) (x) (Just c) (\() -> y)

-- | Low-level button.
button_
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m Bool       -- ^ the button is active when this returns @True@
    -> Modifier m ()        -- ^ the action to do when the button is pressed
    -> Widget m
button_ r x y = return $ Button (r) (x) Nothing (\() -> y)

button
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m (Maybe (Modifier m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget m
button r fm = button_ r (liftM isJust fm) (liftReadRef fm >>= maybe (return ()) id)



smartButton
    :: (EffRef m, EqReference r, RefReader r ~ RefReader (RefCore m)) 
    => ReadRef m String     -- ^ dynamic label of the button
    -> MRef r a              -- ^ underlying reference
    -> (a -> a)   -- ^ The button is active when this function is not identity on value of the reference. When the button is pressed, the value of the reference is modified with this function.
    -> Widget m
smartButton s r f
    = button_ s (hasEffect r f) (modRef r f)

-- | Checkbox.
checkbox :: EffRef m => Ref m Bool -> Widget m
checkbox r = return $ Checkbox ((readRef r), writeRef r)

-- | Simple combo box.
combobox :: EffRef m => [String] -> Ref m Int -> Widget m
combobox ss r = return $ Combobox ss ((readRef r), writeRef r)

-- | Text entry.
entry :: (EffRef m, Reference r, RefReader r ~ RefReader (RefCore m))  => MRef r String -> Widget m
entry r = return $ Entry (const True) ((readRef r), writeRef r)

-- | Text entry.
entryShow :: forall m a r . (EffRef m, Show a, Read a, Reference r, RefReader r ~ RefReader (RefCore m)) => MRef r a -> Widget m
entryShow r_ = return $ Entry isOk ((readRef r), writeRef r)
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
notebook :: EffRef m => [(String, Widget m)] -> Widget m
notebook xs = do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell (liftM (== index) $ readRef currentPage) $ \b -> case b of
           False -> hcat []
           True -> w
    return $ Notebook' (writeRef currentPage) $ zipWith f [0..] xs

{- | Dynamic cell.

The monadic action for inner widget creation is memoised in the first monad layer.
-}
cell_ :: (EffRef m, Eq a) => ReadRef m a -> (forall x . (Widget m -> m x) -> a -> m (m x)) -> Widget m
cell_ r f = return $ Cell r f

{- | Dynamic cell.

The inner widgets are memoised.
-}
cell :: (EffRef m, Eq a) => ReadRef m a -> (a -> Widget m) -> Widget m
cell r m = cell_ r $ \mk -> liftM return . mk . m

{- | Dynamic cell.

The inner widgets are not memoised.
-}
cellNoMemo :: (EffRef m, Eq a) => ReadRef m a -> (a -> Widget m) -> Widget m
cellNoMemo r m = cell_ r $ \mk -> return . mk . m

canvas
    :: (EffRef m, Eq b, Monoid a, Semigroup a)
    => Int   -- ^ width
    -> Int   -- ^ height
    -> Double  -- ^ scale
    -> ((MouseEvent a, Dia a) -> Modifier m ())
    -> KeyboardHandler (Modifier m)
    -> ReadRef m b
    -> (b -> Dia a)
    -> Widget m
canvas w h sc me kh r f = return $ Canvas w h sc me kh r f

hscale
    :: (EffRef m)
    => Double   -- ^ min
    -> Double   -- ^ max
    -> Double   -- ^ step
    -> Ref m Double
    -> Widget m
hscale a b c r = return $ Scale a b c (readRef r, writeRef r)

listLens :: Lens' (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set (_, x) [] = (False, x)
    set _ (l: r) = (True, (l, r))


-- | Undo-redo state transformation.
undoTr
    :: EffRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a             -- ^ reference of state
    ->   m ( ReadRef m (Maybe (Modifier m ()))
           , ReadRef m (Maybe (Modifier m ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extRef r (undoLens eq) ([], [])
    let try f = liftM (liftM (writeRef ku) . f) $ readRef ku
    return (try undo, try redo)
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


