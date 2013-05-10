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

import Control.Monad.Restricted (Morph)
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
    :: forall m . Monad m
    => Morph IO m
    -> (IO () -> IO ())
    -> Morph IO IO
    -> Widget IO m
    -> m SWidget
runWidget liftEffectM post' post = toWidget
 where
    toWidget i = case i of
        Action m -> m >>= toWidget
        Label s -> do
            w <- liftEffectM $ post $ labelNew Nothing
            s $ post . labelSetLabel w
            return' w
        Button s sens m -> do
            w <- liftEffectM $ post buttonNew
            s $ post . buttonSetLabel w
            sens $ post . widgetSetSensitive w
            m $ \x -> void' $ post $ on w buttonActivated $ x ()
            return' w
        Entry (r, s) -> do
            w <- liftEffectM $ post entryNew
            r $ post . entrySetText w
            s $ \re -> void' $ post $ on w entryActivate $ entryGetText w >>= re
            return' w
        Checkbox (r, s) -> do
            w <- liftEffectM $ post checkButtonNew
            r $ post . toggleButtonSetActive w
            s $ \re -> void' $ post $ on w toggled $ toggleButtonGetActive w >>= re
            return' w
        Combobox ss (r, s) -> do
            w <- liftEffectM $ post comboBoxNewText
            liftEffectM $ post $ flip mapM_ ss $ comboBoxAppendText w
            r $ post . comboBoxSetActive w
            s $ \re -> void' $ post $ on w changed $ fmap (max 0) (comboBoxGetActive w) >>= re
            return' w
        List o xs -> do
            ws <- mapM toWidget xs
            w <- liftEffectM $ post $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            shs <- forM ws $ liftEffectM . post . containerAdd'' w . snd
            liftM (mapFst (sequence_ shs >>)) $ return'' ws w
        Notebook' s xs -> do
            ws <- mapM (toWidget . snd) xs
            w <- liftEffectM $ post notebookNew
            forM_ (zip ws xs) $ \(ww, (s, _)) -> do
                liftEffectM . post . flip (notebookAppendPage w) s $ snd $ ww
            s $ \re -> void' $ post $ on w switchPage $ re
            return'' ws w

        Cell onCh f -> do
            let b = False
            w <- liftEffectM $ post $ case b of
                True -> fmap castToContainer $ hBoxNew False 1
                False -> fmap castToContainer $ alignmentNew 0 0 1 1
            sh <- liftEffectM $ liftIO $ newIORef $ return ()
            onCh $ \bv -> do
              x <- toWidget $ f bv
              return $ liftEffectM $ post $ do
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


