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
    , runWidget

    -- ** GUI descriptions
    , Widget
    , action
    , label
    , button_
    , checkbox
    , combobox
    , entry
    , vcat
    , hcat
    , notebook
    , cell

    -- ** Derived constructs
    , labelConst
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

putStrLn_ :: EffIORef m => String -> m ()
putStrLn_ = putStr_ . (++ "\n")


type Widget m = Gtk.Widget (EffectM m) m

vcat :: [Widget m] -> Widget m
vcat = List Vertical

hcat :: [Widget m] -> Widget m
hcat = List Horizontal

smartButton
  :: (EffRef m, Eq a) =>
     ReadRef m String -> (a -> ReadRef m a) -> Ref m a -> Widget m
smartButton s f k =
    button_ s (readRef k >>= \x -> liftM (/= x) $ f x)
             (liftReadPart (readRef k) >>= liftReadPart . f >>= writeRef k)

cell :: (EffRef m, Eq a) => ReadRef m a -> (a -> m (Widget m)) -> Widget m
cell r g = Cell (onChange r) $ Action . g

label :: EffRef m => ReadRef m String -> Widget m
label = Label . rEffect

labelConst :: EffRef m => String -> Widget m
labelConst = label . return

button
    :: EffRef m
    => ReadRef m String
    -> ReadRef m (Maybe (WriteRef m ()))     -- ^ when the @Maybe@ value is @Nothing@, the button is inactive
    -> Widget m
button r fm = button_ r (liftM isJust fm) (liftReadPart fm >>= maybe (return ()) id)

button_
    :: EffRef m
    => ReadRef m String
    -> ReadRef m Bool
    -> WriteRef m ()
    -> Widget m
button_ r x y = Button (rEffect r) (rEffect x) (toReceive $ \() -> y)

checkbox :: EffRef m => Ref m Bool -> Widget m
checkbox r = Checkbox (rEffect (readRef r), toReceive $ writeRef r)

combobox :: EffRef m => [String] -> Ref m Int -> Widget m
combobox ss r = Combobox ss (rEffect (readRef r), toReceive $ writeRef r)

entry :: EffRef m => Ref m String -> Widget m
entry r = Entry (rEffect (readRef r), toReceive $ writeRef r)

notebook :: EffRef m => [(String, m (Widget m))] -> Widget m
notebook xs = Action $ do
    currentPage <- newRef 0
    let f index (title, w) = (,) title $ cell (liftM (== index) $ readRef currentPage) h where
           h False = return $ hcat []
           h True = w
    return $ Notebook' (toReceive $ writeRef currentPage) $ zipWith f [0..] xs

action :: EffRef m => m (Widget m) -> Widget m
action = Action

-- | Run an interface description
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



