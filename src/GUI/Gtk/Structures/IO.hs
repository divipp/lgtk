{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GUI.Gtk.Structures.IO
    ( runWidget
    , gtkContext
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import Data.Maybe
import Data.IORef
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (Widget)
import qualified Graphics.UI.Gtk as Gtk

import Control.Monad.Restricted
import GUI.Gtk.Structures

gtkContext :: (Morph IO IO -> IO SWidget) -> IO ()
gtkContext m = do
    _ <- unsafeInitGUIForThreadedRTS
    tid <- myThreadId
    let post :: Morph IO IO
        post e = do
            tid' <- myThreadId
            if tid' == tid then e else postGUISync e
    c <- m post
    window <- windowNew
    set window [ containerBorderWidth := 10, containerChild := snd c ]
    _ <- window `on` deleteEvent $ liftIO ( mainQuit) >> return False
    widgetShowAll window
    mainGUI

type SWidget = (IO (), Gtk.Widget)

-- | Run an @IO@ parametrized interface description with Gtk backend
runWidget
    :: forall n m . (Monad m, MonadIO n)
    => Morph n m
    -> (IO () -> IO ())
    -> Morph IO IO
    -> Morph n IO
    -> Widget n m
    -> m SWidget
runWidget liftInn post' post dca = toWidget
 where
    liftIO' :: MonadIO n => IO a -> n a
    liftIO' = liftIO . post

    toWidget i = case i of
        Action m -> runC m >>= toWidget
        Label s -> do
            w <- liftInn $ liftIO' $ labelNew Nothing
            s $ liftIO' . labelSetLabel w
            return' w
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
            ws <- mapM toWidget xs
            w <- liftInn $ liftIO' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            shs <- forM ws $ liftInn . liftIO' . containerAdd'' w . snd
            liftM (mapFst (sequence_ shs >>)) $ return'' ws w
        Notebook' s xs -> do
            ws <- mapM (toWidget . snd) xs
            w <- liftInn $ liftIO' notebookNew
            forM_ (zip ws xs) $ \(ww, (s, _)) -> do
                liftInn . liftIO' . flip (notebookAppendPage w) s $ snd $ ww
            s $ \re -> void' $ liftIO' $ on w switchPage $ dca . re
            return'' ws w

        Cell' f -> do
            let b = False
            w <- liftInn $ liftIO' $ case b of
                True -> fmap castToBox $ hBoxNew False 1
                False -> fmap castToBox $ alignmentNew 0 0 1 1
            sh <- liftInn $ liftIO $ newIORef $ return ()
            f (unsafeC . toWidget) $ \x -> liftIO' $ do
                writeIORef sh $ fst x
                post' $ post $ fst x
                containerForeach w $ if b then widgetHideAll else containerRemove w 
                ch <- containerGetChildren w
                when (snd x `notElem` ch) $ containerAdd w $ snd x
            liftM (mapFst (join (readIORef sh) >>)) $ return'' [] w

return' :: Monad m => WidgetClass x => x -> m SWidget
return' w = return (widgetShowAll w, castToWidget w)

return'' :: Monad m => WidgetClass x => [SWidget] -> x -> m SWidget
return'' ws w = return (mapM_ fst ws >> widgetShow w, castToWidget w)

void' :: Monad m => m a -> m ()
void' m = m >> return ()

mapFst f (a, b) = (f a, b)

containerAdd'' w x = do
    a <- alignmentNew 0 0 0 0
    containerAdd a x
    containerAdd w a
    set w [ boxChildPacking a := PackNatural ]
    return $ widgetShow a


