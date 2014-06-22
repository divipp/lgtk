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
    -- * GUI

    -- ** GUI elements
      empty
    , vcat
    , hcat
    , label
    , button
    , smartButton
    , button_
    , button__
    , checkbox
    , combobox
    , entry
    , entryShow
    , entryPrism
    , hscale
    , cell
    , cellNoMemo
    , notebook
    , canvas
    , inCanvas

    -- ** Types
    , Widget
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
    , undoPush
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

    -- ** Reference creation
    , unitRef
    , newRef
    , extRef
    , lensMap

    , newEqRef
    , toEqRef
    , fromEqRef

    -- ** Triggers
    , onChange
    , onChangeEq
    , onChangeEq_
    , onChangeMemo

    -- ** Other
    , memoRead

    -- ** Reference writing
    , writeRef
    , modRef

    -- ** Reference reading
    , readRef
    , liftRefReader

    -- ** Types
    , RefCreator
    , RefReader
    , RefWriter
    , Ref
    , EqRef

    -- ** Other types and type classes
    , RefSimple
    , RefReaderSimple
--    , EqRefClass
    , RefClass

    ) where

--import Data.String
import Control.Applicative hiding (empty)
import Control.Lens
import Control.Lens.Extras (is)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Semigroup

import Data.LensRef (RefClass (RefReaderSimple), RefSimple, hasEffect, toEqRef, fromEqRef, RefReaderOf, MonadRefReader, BaseRef)
import qualified Data.LensRef as Ref
import LGtk.Effects ()
import qualified LGtk.Effects as Eff
import LGtk.Widgets hiding (Widget)
import LGtk.Render
import LGtk.Key

#ifdef __GTK__
import LGtk.Backend.Gtk
#else
import LGtk.Backend.GLFW
#endif

----------------------------

readRef
    :: ( MonadRefReader m
       , RefClass r
       , RefReaderOf m ~ RefReaderSimple r
       , BaseRef m ~ BaseRef (Ref.RefWriterOf m)
       )
    => RefSimple r a -> m a
readRef = Ref.readRef

liftRefReader
    :: ( MonadRefReader m
       , BaseRef m ~ BaseRef (Ref.RefWriterOf m)
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

modRef :: (RefClass r, RefReaderSimple r ~ RefReader) => RefSimple r a -> (a -> a) -> RefWriter ()
modRef = Ref.modRef

extRef :: Ref b -> Lens' a b -> a -> RefCreator (Ref a)
extRef = Ref.extRef

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

memoRead :: RefCreator a -> RefCreator (RefCreator a)
memoRead = Ref.memoRead

newEqRef :: Eq a => a -> RefCreator (EqRef a)
newEqRef = Ref.newEqRef

getArgs :: RefCreator [String]
getArgs = Eff.getArgs

getProgName :: RefCreator String
getProgName = Eff.getProgName

lookupEnv :: String -> RefCreator (Maybe String)
lookupEnv = Eff.lookupEnv

asyncWrite :: Int -> RefWriter () -> RefCreator ()
asyncWrite = Eff.asyncWrite

putStr_ :: String -> RefCreator ()
putStr_ = Eff.putStr_

putStrLn_ :: String -> RefCreator ()
putStrLn_ = Eff.putStrLn_

getLine_ :: (String -> RefWriter ()) -> RefCreator ()
getLine_ = Eff.getLine_

fileRef :: FilePath -> RefCreator (Ref (Maybe String))
fileRef = Eff.fileRef



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
    :: RefReader String     -- ^ dynamic label of the button
    -> EqRef a              -- ^ underlying reference
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

-- | String entry.
entry :: (RefClass r, RefReaderSimple r ~ RefReader)  => RefSimple r String -> Widget
entry = entryPrism id

entryShow :: forall a r . (Show a, Read a, RefClass r, RefReaderSimple r ~ RefReader) => RefSimple r a -> Widget
entryShow = entryPrism _Show

entryPrism :: forall a r . (RefClass r, RefReaderSimple r ~ RefReader) => Prism' String a -> RefSimple r a -> Widget
entryPrism prism r = return $ Entry (is prism) (getContent, changed)
  where
    getContent = review prism <$> readRef r
    changed x = forM_ (x ^? prism) (writeRef r)

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

undoPush
    :: (a -> a -> Bool)
    -> Ref a
    -> RefCreator
           ( RefReader (Maybe (RefWriter ()))
           , RefReader (Maybe (RefWriter ()))
           , RefWriter ()
           ) -- ^ undo action, redo action, push state action
undoPush eq ref = do
    initial <- readRef ref
    -- This isn't an extRef, because state pushing is requested
    -- explicitly.
    state <- newRef ([], initial, [])
    let undo = do
            val <- readRef state
            case val of
                ([], _, _) -> pure Nothing
                (u:us', x, rs) -> pure $ Just $ do
                    writeRef state (us', u, x:rs)
                    writeRef ref u
        redo = do
            val <- readRef state
            case val of
                (_, _, []) -> pure Nothing
                (us, x, r:rs') -> pure $ Just $ do
                    writeRef state (x:us, r, rs')
                    writeRef ref r
        push = do
            x' <- readRef ref
            modRef state $ \unchanged@(us, x, _) ->
                if x `eq` x'
                    then unchanged
                    else (x:us, x', [])
    pure (undo, redo, push)
