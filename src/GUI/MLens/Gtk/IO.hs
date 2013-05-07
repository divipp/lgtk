{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GUI.MLens.Gtk.IO
    ( runWidget
    , gtkContext
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import Data.Maybe
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (Widget)
import qualified Graphics.UI.Gtk as Gtk

import Control.Monad.Restricted
import GUI.MLens.Gtk.Interface

gtkContext :: (Morph IO IO -> IO Gtk.Widget) -> IO ()
gtkContext m = do
    _ <- unsafeInitGUIForThreadedRTS
    tid <- myThreadId
    let post :: Morph IO IO
        post e = do
            tid' <- myThreadId
            if tid' == tid then e else postGUISync e
    c <- m post
    window <- windowNew
    set window [ containerBorderWidth := 10, containerChild := c ]
    _ <- window `on` deleteEvent $ liftIO ( mainQuit) >> return False
    widgetShowAll window
    mainGUI

-- | Run an @IO@ parametrized interface description with Gtk backend
runWidget
    :: forall n m . (Monad m, MonadIO n)
    => Morph n m
    -> (IO () -> IO ())
    -> Morph IO IO
    -> Morph n IO
    -> Widget n m
    -> m Gtk.Widget
runWidget liftInn post' post dca = toWidget
 where
    liftIO' :: MonadIO n => IO a -> n a
    liftIO' = liftIO . post

    toWidget i = case i of
        Button s sens m -> do
            w <- liftInn $ liftIO' buttonNew
            s $ liftIO' . buttonSetLabel w
            sens $ liftIO' . widgetSetSensitive w
            m $ \x -> liftIO' $ void' $ on w buttonActivated $ dca $ x ()
            return' w
        Entry (r, s) -> do
            w <- liftInn $ liftIO' entryNew
            r $ liftIO' . entrySetText w
            s $ \re -> void' $ liftIO' $ on w entryActivate $ entryGetText w >>= dca . re
            return' w
        Checkbox (r, s) -> do
            w <- liftInn $ liftIO' checkButtonNew
            r $ liftIO' . toggleButtonSetActive w
            s $ \re -> void' $ liftIO' $ on w toggled $ toggleButtonGetActive w >>= dca . re
            return' w
        Combobox ss (r, s) -> do
            w <- liftInn $ liftIO' comboBoxNewText
            liftInn $ liftIO' $ flip mapM_ ss $ comboBoxAppendText w
            r $ liftIO' . comboBoxSetActive w
            s $ \re -> void' $ liftIO' $ on w changed $ fmap (max 0) (comboBoxGetActive w) >>= dca . re
            return' w
        List o xs -> do
            w <- liftInn $ liftIO' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            flip mapM_ xs $ toWidget >=> liftInn . liftIO' . containerAdd'' w
            return' w
        Notebook' s xs -> do
            w <- liftInn $ liftIO' notebookNew
            forM_ xs $ \(s, i) -> do
                ww <- toWidget i
                liftInn . liftIO' . flip (notebookAppendPage w) s $ ww
            s $ \re -> void' $ liftIO' $ on w switchPage $ dca . re
            return' w
        Label s -> do
            w <- liftInn $ liftIO' $ labelNew Nothing
            s $ liftIO' . labelSetLabel w
            return' w
        Action m -> 
            runC m >>= toWidget
        Cell' f -> do
            w <- liftInn $ liftIO' $ alignmentNew 0 0 1 1
--            w <- lift $ hBoxNew False 1
            f (unsafeC . toWidget) $ \x -> liftIO' $ do
--                containerForeach w $ widgetHideAll
                containerForeach w $ containerRemove w
                containerAdd w x
                post' $ post $ widgetShowAll x
            return' w
        Cell'' f -> do
            w <- liftInn $ liftIO' $ alignmentNew 0 0 1 1
            f (unsafeC . toWidget) $ \x -> liftIO' $ case x of
              Nothing -> return ()
              Just x -> do
                ch <- containerGetChildren w
                case ch of
                    [y] | y == x -> return ()
                    _ -> do
                        containerForeach w $ containerRemove w
                        containerAdd w x
                        post' $ post $ widgetShowAll x
            return' w

return' :: Monad m => GObjectClass x => x -> m Gtk.Widget
return' = return . castToWidget

void' :: Monad m => m a -> m ()
void' m = m >> return ()

containerAdd'' w x = do
    a <- alignmentNew 0 0 0 0
    containerAdd a x
    containerAdd w a
    set w [ boxChildPacking a := PackNatural ]


