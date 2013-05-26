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
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (Widget)
import qualified Graphics.UI.Gtk as Gtk

import Control.Monad.Restricted (Morph)
import Control.Monad.Register (Command (..))
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
    :: forall n m . (MonadIO m, MonadIO n)
    => Morph n IO
    -> (IO () -> IO ())
    -> Morph IO IO
    -> Widget n m
    -> m SWidget
runWidget nio post' post = toWidget
 where
    liftIO' :: MonadIO k => IO a -> k a
    liftIO' = liftIO . post

    reg :: Receive n m a -> Receive IO m a
    reg s f = s $ liftM (fmap liftIO) . liftIO' . f . (nio .)

    ger :: Send n m a -> Send IO m a
    ger s f = s $ liftIO' . f

    toWidget :: Widget n m -> m SWidget
    toWidget i = case i of

        Action m -> m >>= toWidget
        Label s -> do
            w <- liftIO' $ labelNew Nothing
            ger s $ labelSetLabel w
            return' w
        Button s sens m -> do
            w <- liftIO' buttonNew
            ger s $ buttonSetLabel w
            ger sens $ widgetSetSensitive w
            reg m $ \x -> on' w buttonActivated $ x ()
            return' w
        Entry (r, s) -> do
            w <- liftIO' entryNew
            ger r $ entrySetText w
            reg s $ \re -> on' w entryActivate $ entryGetText w >>= re
            return' w
        Checkbox (r, s) -> do
            w <- liftIO' checkButtonNew
            ger r $ toggleButtonSetActive w
            reg s $ \re -> on' w toggled $ toggleButtonGetActive w >>= re
            return' w
        Combobox ss (r, s) -> do
            w <- liftIO' comboBoxNewText
            liftIO' $ flip mapM_ ss $ comboBoxAppendText w
            ger r $ comboBoxSetActive w
            reg s $ \re -> on' w changed $ fmap (max 0) (comboBoxGetActive w) >>= re
            return' w
        List o xs -> do
            ws <- mapM toWidget xs
            w <- liftIO' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            shs <- forM ws $ liftIO' . containerAdd'' w . snd
            liftM (mapFst (sequence_ shs >>)) $ return'' ws w
        Notebook' s xs -> do
            ws <- mapM (toWidget . snd) xs
            w <- liftIO' notebookNew
            forM_ (zip ws xs) $ \(ww, (s, _)) -> do
                liftIO' . flip (notebookAppendPage w) s $ snd $ ww
            reg s $ \re -> on' w switchPage $ re
            return'' ws w
        Cell onCh f -> do
            let b = False
            w <- liftIO' $ case b of
                True -> fmap castToContainer $ hBoxNew False 1
                False -> fmap castToContainer $ alignmentNew 0 0 1 1
            sh <- liftIO $ liftIO $ newMVar $ return ()
            onCh $ \bv -> do
              mx <- f toWidget bv
              return $ do
                x <- mx
                liftIO' $ do 
                  _ <- swapMVar sh $ fst x
                  post' $ post $ fst x
                  containerForeach w $ if b then widgetHideAll else containerRemove w 
                  ch <- containerGetChildren w
                  when (snd x `notElem` ch) $ containerAdd w $ snd x
            liftM (mapFst (join (readMVar sh) >>)) $ return'' [] w

on' :: GObjectClass x => x -> Signal x c -> c -> IO (Command -> IO ())
on' o s c = liftM (flip f) $ on o s c where
    f Kill = signalDisconnect
    f Block = signalBlock
    f Unblock = signalUnblock

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


