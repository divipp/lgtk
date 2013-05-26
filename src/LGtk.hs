{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Main LGtk interface.
module LGtk
    ( 
    -- * "Control.Category" re-export
      (.)
    , id

    -- * Lenses
    -- ** "Data.Lens.Common" module
    , module Data.Lens.Common

    -- ** Additional lenses
    , listLens
    , maybeLens

    -- ** Impure lenses
    , showLens

    -- * Auxiliary definitions
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
    , readRef'

    -- ** Derived constructs
    , modRef
    , memoRead
    , undoTr

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

    -- ** Derived
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
    , cell
    , action

    -- ** Derived constructs
    , button
    , smartButton

    ) where

import Data.Maybe
import Control.Category
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

cell :: (EffRef m, Eq a) => ReadRef m a -> (a -> m (Widget m)) -> Widget m
cell r g = Cell (onChange r) $ Action . g

-- | Dynamic label.
label :: EffRef m => ReadRef m String -> Widget m
label = Label . rEffect

-- | Low-level button.
button_
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m Bool       -- ^ the button is active when this returns @True@
    -> WriteRef m ()        -- ^ the action to do when the button is pressed
    -> Widget m
button_ r x y = Button (rEffect r) (rEffect x) (toReceive $ \() -> y)

button
    :: EffRef m
    => ReadRef m String     -- ^ dynamic label of the button
    -> ReadRef m (Maybe (WriteRef m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget m
button r fm = button_ r (liftM isJust fm) (liftReadPart fm >>= maybe (return ()) id)

smartButton
    :: (EffRef m, Eq a)
    => ReadRef m String     -- ^ dynamic label of the button
    -> Ref m a              -- ^ underlying reference
    -> (a -> ReadRef m a)   -- ^ The button is active when this monadic function changes its input. When the button is pressed, the value of the reference is modified with this function.
    -> Widget m
smartButton s k f =
    button_ s (readRef k >>= \x -> liftM (/= x) $ f x)
             (liftReadPart (readRef k) >>= liftReadPart . f >>= writeRef k)

-- | Checkbox.
checkbox :: EffRef m => Ref m Bool -> Widget m
checkbox r = Checkbox (rEffect (readRef r), toReceive $ writeRef r)

-- | Simple combo box.
combobox :: EffRef m => [String] -> Ref m Int -> Widget m
combobox ss r = Combobox ss (rEffect (readRef r), toReceive $ writeRef r)

-- | Text entry.
entry :: EffRef m => Ref m String -> Widget m
entry r = Entry (rEffect (readRef r), toReceive $ writeRef r)

-- | Notebook (tabs).
notebook :: EffRef m => [(String, m (Widget m))] -> Widget m
notebook xs = Action $ do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell (liftM (== index) $ readRef currentPage) h where
           h False = return $ hcat []
           h True = w
    return $ Notebook' (toReceive $ writeRef currentPage) $ zipWith f [0..] xs

-- | @action@ makes possible to do any 'EffRef' action while creating the widget.
action :: EffRef m => m (Widget m) -> Widget m
action = Action




