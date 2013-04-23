{-# LANGUAGE RankNTypes #-}
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
runI :: I IO -> IO ()
runI i = do 
    c <- evalEE_ $ \dca -> do
        _ <- liftInn $ initGUI
        rea <- runC $ newRef True
        userr_ rea dca i
    window <- windowNew
    set window [ containerBorderWidth := 10, containerChild := c ]
    _ <- window `on` deleteEvent $ liftIO (mainQuit) >> return False
    widgetShowAll window
    mainGUI
 where
    userr_ :: Ref IO Bool -> Morph (EE IO) IO -> I IO -> EE IO Widget
    userr_ rea dca i = case i of
        Button s m -> do
            w <- lift buttonNew
            addFreeCEffect s $ buttonSetLabel w
            addFreeCEffect (fmap isJust m) $ widgetSetSensitive w
            addPushEffect (unFree (maybe (return ()) id) (join . fmap (maybe (return ()) id) . runC) m) $ \x -> on w buttonActivated (dca x) >> return ()
            return' w
        Entry k -> do
            w <- lift entryNew
            addRefEffect k $ \re -> do
                _ <- on w entryActivate $ entryGetText w >>= dca . re
                return $ entrySetText w
            return' w
        Checkbox k -> do
            w <- lift checkButtonNew
            addRefEffect k $ \re -> do
                _ <- on w toggled $ toggleButtonGetActive w >>= dca . re
                return $ toggleButtonSetActive w
            return' w
        Combobox ss k -> do
            w <- lift comboBoxNewText
            lift $ flip mapM_ ss $ comboBoxAppendText w
            addRefEffect k $ \re -> do
                _ <- on w changed $ fmap (max 0) (comboBoxGetActive w) >>= dca . re
                return $ comboBoxSetActive w
            return' w
        List o xs -> do
            w <- lift $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            flip mapM_ xs $ flattenI' >=> containerAdd'' w
            return' w
        Notebook xs -> do
            w <- lift notebookNew
            flip mapM_ xs $ \(s, i) ->
                flattenI' i >>= lift . flip (notebookAppendPage w) s
            return' w
        Label s -> do
            w <- lift $ labelNew Nothing
            addFreeCEffect s $ labelSetLabel w
            return' w
        Action m -> 
            lift (runC m) >>= flattenI'
        Cell b m f -> do
            w <- lift $ alignmentNew 0 0 1 1
--            w <- lift $ hBoxNew False 1
            (if b then addMemoICEffect else addICEffect) (IC m $ \b -> unsafeC $ flattenI' $ f b) $ return $ \x -> do
--                containerForeach w $ widgetHideAll
                containerForeach w $ containerRemove w
                containerAdd w x
                widgetShowAll w
            return' w
      where
        flattenI' = userr_ rea dca

    return' :: GObjectClass x => x -> EE IO Widget
    return' = return . castToWidget

    lift = liftInn

    containerAdd'' w x = lift $ do
        a <- alignmentNew 0 0 0 0
        containerAdd a x
        containerAdd w a
        set w [ boxChildPacking a := PackNatural ]

