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
import Data.Maybe
import Prelude hiding ((.), id)

import Graphics.UI.Gtk

import Control.Monad.Restricted
import Control.Monad.Register
import Control.MLens
import Control.MLens.Unsafe ()
import GUI.MLens.Gtk.Interface

gtkContext m = do
    _ <- unsafeInitGUIForThreadedRTS
    c <- m
    window <- windowNew
    set window [ containerBorderWidth := 10, containerChild := c ]
    _ <- window `on` deleteEvent $ liftIO'' mainQuit >> return False
    widgetShowAll window
    mainGUI

-- | Run an @IO@ parametrized interface description with Gtk backend
runI
    :: forall m . (MonadRegister m, MonadIO (Inn m), Functor (Inner m))
    => Morph (Inn m) IO
    -> I m
    -> m Widget
runI dca i = toWidget i
--    update  -- ???
 where
    toWidget :: I m -> m Widget
    toWidget i = case i of
        Button s m -> do
            w <- liftInn $ liftIO'' buttonNew
            s $ liftIO' . buttonSetLabel w
            addFreeCEffect (fmap isJust m) $ liftIO' . widgetSetSensitive w
            addPushEffect (unFree (maybe (return ()) id) (join . fmap (maybe (return ()) id) . runR) m) $ \x -> liftIO'' $ on w buttonActivated (dca x) >> return ()
            return' w
        Entry k -> do
            w <- liftInn $ liftIO'' entryNew
            addRefEffect k $ \re -> do
                _ <- liftIO'' $ on w entryActivate $ liftIO'' (entryGetText w) >>= dca . re
                return $ liftIO' . entrySetText w
            return' w
        Checkbox k -> do
            w <- liftInn $ liftIO'' checkButtonNew
            addRefEffect k $ \re -> do
                _ <- liftIO'' $ on w toggled $ liftIO'' (toggleButtonGetActive w) >>= dca . re
                return $ liftIO' . toggleButtonSetActive w
            return' w
        Combobox ss k -> do
            w <- liftInn $ liftIO'' comboBoxNewText
            liftInn $ liftIO'' $ flip mapM_ ss $ comboBoxAppendText w
            addRefEffect k $ \re -> do
                _ <- liftIO'' $ on w changed $ fmap (max 0) (liftIO'' $ comboBoxGetActive w) >>= dca . re
                return $ liftIO' . comboBoxSetActive w
            return' w
        List o xs -> do
            w <- liftInn $ liftIO'' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            flip mapM_ xs $ toWidget >=> liftInn . liftIO'' . containerAdd'' w
            return' w
        Notebook xs -> do
            currentPage <- runC $ newRef 0
            w <- liftInn $ liftIO'' notebookNew
            forM_ (zip [0..] xs) $ \(index, (s, i)) -> do
                ww <- liftInn $ liftIO'' $ alignmentNew 0 0 1 1
                let f True = liftM Just $ unsafeC $ toWidget i
                    f False = return Nothing
                    g Nothing = return ()
                    g (Just x) = liftIO'' $ do
                        ch <- containerGetChildren ww
                        when (null ch) $ liftIO' $ do
                            containerAdd ww x
                            widgetShowAll ww
                addICEffect True (IC (fmap (== index) $ readRef currentPage) f) g
                liftInn . liftIO'' . flip (notebookAppendPage w) s $ ww
            addWEffect (writeRef currentPage) $ \re -> do
                _ <- liftIO'' $ on w switchPage $ dca . re
                return ()
            return' w
        Label s -> do
            w <- liftInn $ liftIO'' $ labelNew Nothing
            s $ \lab -> liftIO $ do
                liftIO' $ do
                    labelSetLabel w lab
            return' w
        Action m -> 
            runC m >>= toWidget
        Cell b (IC m f) -> do
            w <- liftInn $ liftIO'' $ alignmentNew 0 0 1 1
--            w <- lift $ hBoxNew False 1
            addICEffect b (IC m $ f >=> unsafeC . toWidget) $ \x -> liftIO' $ do
--                containerForeach w $ widgetHideAll
                containerForeach w $ containerRemove w
                containerAdd w x
                widgetShowAll w
            return' w
        Cell' f -> do
            w <- liftInn $ liftIO'' $ alignmentNew 0 0 1 1
--            w <- lift $ hBoxNew False 1
            f (unsafeC . toWidget) $ \x -> liftIO' $ do
--                containerForeach w $ widgetHideAll
                containerForeach w $ containerRemove w
                containerAdd w x
                widgetShowAll w
            return' w

liftIO' :: MonadIO m => IO a -> m a
liftIO' m = liftIO $ postGUISync m

liftIO'' :: MonadIO m => IO a -> m a
liftIO'' m = liftIO m  -- liftIO' m -- ???

return' :: Monad m => GObjectClass x => x -> m Widget
return' = return . castToWidget

containerAdd'' w x = do
    a <- alignmentNew 0 0 0 0
    containerAdd a x
    containerAdd w a
    set w [ boxChildPacking a := PackNatural ]


