{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Main LGtk interface.
module LGtk
    ( 
    -- * Categories
      Category (..)
    , Tensor (..)
    , liftM

    -- * Lenses
    -- ** Construction
    , Lens (Lens)
    , lens
    , iso

    -- ** Deconstruction
    , runLens
    , getL
    , setL
    , modL

    -- ** Pure lenses
    , fstLens
    , sndLens
    , listLens
    , maybeLens

    -- ** Impure lenses
    , showLens

    -- * Monad morphisms
    , Morph
    , HasReadPart (..)

    -- * References
    -- ** Basic operations
    , Reference
    , RefMonad
    , readRef
    , writeRef
    , lensMap
    , joinRef
    , unitRef

    -- ** Reference creation
    , ExtRef
    , Ref
    , extRef
    , newRef
    , ReadRef
    , WriteRef
    , liftReadRef

    -- ** Derived constructs
    , modRef
    , readRef'
    , memoRead
    , undoTr

    , EqRef
    , eqRef
    , toRef
    , hasEffect

    -- * Dynamic networks
    , EffRef
    , onChange

    -- * I/O
    , SafeIO
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
    , runWidget

    -- ** GUI descriptions
    , label
    , button_
    , checkbox
    , combobox
    , entry
    , vcat
    , hcat
    , notebook
    , cell_
    , action

    -- ** Derived constructs
    , empty
    , entryShow
    , button
    , smartButton
    , cell
    , cellNoMemo

    ) where

import Data.Maybe
import Control.Category
import Control.Category.Product
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Identity
import Prelude hiding ((.), id)
import Data.Lens.Common

import Control.Monad.ExtRef
import Control.Monad.Register
import Control.Monad.Register.Basic
import Control.Monad.EffRef
import GUI.Gtk.Structures hiding (Send, Receive, SendReceive, Widget)
import qualified GUI.Gtk.Structures as Gtk
import qualified GUI.Gtk.Structures.IO as Gtk
import Control.Monad.ExtRef.Pure
import Control.Monad.Restricted


{- |
Gtk widget descriptions.
Construction of a @(w :: forall m . EffIORef m => Widget m)@ value is side-effect free,
side-effects happen at running @('runWidget' w)@.

@Widget m@ should be abstract, but it is also safe to keep it as a type synonym because
the operations of the revealed implementation are hidden.
-}
type Widget m = Gtk.Widget (EffectM m) m

{- |
Run a Gtk widget description.

The widget is shown in a window and the thread enters into the Gtk event cycle.
It leaves the event cycle when the window is closed.
-}
runWidget :: (forall m . EffIORef m => Widget m) -> IO ()
runWidget e = do
    post_ <- newRef' $ return ()
    let post' = runMorphD post_ . modify . flip (>>)
    ch <- newChan
    _ <- forkIO $ forever $ do
        join $ readChan ch
        join $ runMorphD post_ $ state $ \m -> (m, return ())
    Gtk.gtkContext $ \post ->
        runExtRef_ $ unliftIO $ \u ->
            evalRegister
                (        runIdentityT $
                    Gtk.runWidget u post' post e)
                (liftIO . writeChan ch . u)

-- | Vertical composition of widgets.
vcat :: [Widget m] -> Widget m
vcat = List Vertical

-- | Horizontal composition of widgets.
hcat :: [Widget m] -> Widget m
hcat = List Horizontal

-- | Empty widget.
empty :: Widget m
empty = hcat []

-- | Dynamic label.
label :: EffRef m => ReadRef m String -> Widget m
label = Label . rEffect True

-- | Low-level button.
button_
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m Bool       -- ^ the button is active when this returns @True@
    -> WriteRef m ()        -- ^ the action to do when the button is pressed
    -> Widget m
button_ r x y = Button (rEffect True r) (rEffect True x) (toReceive $ \() -> y)

button
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m (Maybe (WriteRef m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget m
button r fm = button_ r (liftM isJust fm) (liftReadPart fm >>= maybe (return ()) id)



smartButton
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> EqRef (Ref m) a              -- ^ underlying reference
    -> (a -> a)   -- ^ The button is active when this function is not identity on value of the reference. When the button is pressed, the value of the reference is modified with this function.
    -> Widget m
smartButton s m f
    = button_ s (hasEffect m f) (modRef m f)

-- | Checkbox.
checkbox :: EffRef m => Ref m Bool -> Widget m
checkbox r = Checkbox (rEffect True (readRef r), toReceive $ writeRef r)

-- | Simple combo box.
combobox :: EffRef m => [String] -> Ref m Int -> Widget m
combobox ss r = Combobox ss (rEffect True (readRef r), toReceive $ writeRef r)

-- | Text entry.
entry :: EffRef m => Ref m String -> Widget m
entry r = Entry (rEffect True (readRef r), toReceive $ writeRef r)

-- | Text entry.
entryShow :: (EffRef m, Show a, Read a) => Ref m a -> Widget m
entryShow r = entry $ showLens `lensMap` r

{- | Notebook (tabs).

The tabs are created lazily.
-}
notebook :: EffRef m => [(String, Widget m)] -> Widget m
notebook xs = Action $ do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell (liftM (== index) $ readRef currentPage) $ \b -> case b of
           False -> hcat []
           True -> w
    return $ Notebook' (toReceive $ writeRef currentPage) $ zipWith f [0..] xs

{- | Dynamic cell.

The monadic action for inner widget creation is memoised in the first monad layer.
-}
cell_ :: (EffRef m, Eq a) => ReadRef m a -> (forall x . (Widget m -> m x) -> a -> m (m x)) -> Widget m
cell_ = Cell . onChange True

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

-- | @action@ makes possible to do any 'EffRef' action while creating the widget.
action :: EffRef m => m (Widget m) -> Widget m
action = Action




