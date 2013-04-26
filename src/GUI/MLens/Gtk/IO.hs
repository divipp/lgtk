{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GUI.MLens.Gtk.IO
    ( runI
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

-- | Run an @IO@ parametrized interface description with Gtk backend
runI
    :: forall m . (MonadRegister m, MonadIO m, MonadIO (Inn m), Functor (Inner m))
    => Morph (Inn m) IO
    -> I m
    -> m ()
runI dca i = do
    _ <- liftIO $ initGUI
    rea <- runC $ newRef True       -- TODO: use this or delete this
    c <- userr_ rea i
    update
    liftIO $ do
        window <- windowNew
        set window [ containerBorderWidth := 10, containerChild := c ]
        _ <- window `on` deleteEvent $ liftIO (mainQuit) >> return False
        widgetShowAll window
        mainGUI
 where
    userr_ :: IRef m Bool -> I m -> m Widget
    userr_ rea i = case i of
        Button s m -> do
            w <- liftIO buttonNew
            addFreeCEffect s $ liftIO . buttonSetLabel w
            addFreeCEffect (fmap isJust m) $ liftIO . widgetSetSensitive w
            addPushEffect (unFree (maybe (return ()) id) (join . fmap (maybe (return ()) id) . runC) m) $ \x -> liftIO $ on w buttonActivated (dca x) >> return ()
            return' w
        Entry k -> do
            w <- liftIO entryNew
            addRefEffect k $ \re -> do
                _ <- liftIO $ on w entryActivate $ entryGetText w >>= dca . re
                return $ liftIO . entrySetText w
            return' w
        Checkbox k -> do
            w <- liftIO checkButtonNew
            addRefEffect k $ \re -> do
                _ <- liftIO $ on w toggled $ toggleButtonGetActive w >>= dca . re
                return $ liftIO . toggleButtonSetActive w
            return' w
        Combobox ss k -> do
            w <- liftIO comboBoxNewText
            liftIO $ flip mapM_ ss $ comboBoxAppendText w
            addRefEffect k $ \re -> do
                _ <- liftIO $ on w changed $ fmap (max 0) (comboBoxGetActive w) >>= dca . re
                return $ liftIO . comboBoxSetActive w
            return' w
        List o xs -> do
            w <- liftIO $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            flip mapM_ xs $ flattenI' >=> containerAdd'' w
            return' w
        Notebook xs -> do
            w <- liftIO notebookNew
            flip mapM_ xs $ \(s, i) ->
                flattenI' i >>= liftIO . flip (notebookAppendPage w) s
            return' w
        Label s -> do
            w <- liftIO $ labelNew Nothing
            addFreeCEffect s $ liftIO . labelSetLabel w
            return' w
        Action m -> 
            runC m >>= flattenI'
        Cell b m f -> do
            w <- liftIO $ alignmentNew 0 0 1 1
--            w <- lift $ hBoxNew False 1
            (if b then addMemoICEffect else addICEffect) (IC m $ \b -> unsafeC $ flattenI' $ f b) $
              return $ \x -> liftIO $ do
--                containerForeach w $ widgetHideAll
                containerForeach w $ containerRemove w
                containerAdd w x
                widgetShowAll w
            return' w
      where
        flattenI' = userr_ rea

    return' :: GObjectClass x => x -> m Widget
    return' = return . castToWidget

    containerAdd'' w x = liftIO $ do
        a <- alignmentNew 0 0 0 0
        containerAdd a x
        containerAdd w a
        set w [ boxChildPacking a := PackNatural ]


