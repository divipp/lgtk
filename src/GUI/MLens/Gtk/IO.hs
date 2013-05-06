{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GUI.MLens.Gtk.IO
    ( runI
    , gtkContext
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import Data.Maybe
import Prelude hiding ((.), id)

import Graphics.UI.Gtk

import Control.Monad.Restricted
import Control.Monad.Register
import Control.MLens
import Control.MLens.Unsafe ()
import GUI.MLens.Gtk.Interface

gtkContext :: (Morph IO IO -> IO Widget) -> IO ()
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
    return ()

-- | Run an @IO@ parametrized interface description with Gtk backend
runI
    :: forall m . (MonadRegister m, ExtRef m, MonadIO (Inn m), Inner m ~ Inner' m, Functor (Inner m))
    => Morph IO IO
    -> Morph (Inn m) IO
    -> I m
    -> m Widget
runI post dca i = do
    w <- toWidget i
    return w
 where
    liftIO' :: MonadIO n => IO a -> n a
    liftIO' = liftIO . post

    toWidget :: I m -> m Widget
    toWidget i = case i of
        Button s sens m -> do
            w <- liftInn $ liftIO' buttonNew
            s $ liftIO' . buttonSetLabel w
            sens $ liftIO' . widgetSetSensitive w
            m $ \x -> liftIO' $ on w buttonActivated (dca $ x ()) >> return ()
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
        Notebook xs -> do
            currentPage <- runC $ newRef 0
            w <- liftInn $ liftIO' notebookNew
            forM_ (zip [0..] xs) $ \(index, (s, i)) -> do
                ww <- liftInn $ liftIO' $ alignmentNew 0 0 1 1
                let f True = liftM Just $ unsafeC $ toWidget i
                    f False = return Nothing
                    g Nothing = return ()
                    g (Just x) = liftIO' $ do
                        ch <- containerGetChildren ww
                        when (null ch) $ do
                            containerAdd ww x
                            widgetShowAll ww
                addICEffect True (IC (fmap (== index) $ readRef currentPage) f) g
                liftInn . liftIO' . flip (notebookAppendPage w) s $ ww
            addWEffect (writeRef currentPage) $ \re -> do
                _ <- liftIO' $ on w switchPage $ dca . re
                return ()
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
                widgetShowAll w
            return' w

return' :: Monad m => GObjectClass x => x -> m Widget
return' = return . castToWidget

void' :: Monad m => m a -> m ()
void' m = m >> return ()

containerAdd'' w x = do
    a <- alignmentNew 0 0 0 0
    containerAdd a x
    containerAdd w a
    set w [ boxChildPacking a := PackNatural ]


