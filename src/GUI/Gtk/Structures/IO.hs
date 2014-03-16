{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GUI.Gtk.Structures.IO
    ( runWidget
    , gtkContext
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe
import Data.List hiding (union)
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (Widget, Release)
import qualified Graphics.UI.Gtk as Gtk
--import Graphics.UI.Gtk.Gdk.Events (eventKeyChar)

import Control.Monad.Restricted (Morph)
import Control.Monad.Register (Command (..))
import GUI.Gtk.Structures

import Diagrams.Prelude hiding (Widget)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

-------------------------

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
    _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll window
    mainGUI

type SWidget = (IO (), Gtk.Widget)

-- | Run an @IO@ parametrized interface description with Gtk backend
runWidget
    :: forall n m k o . (Monad m, MonadIO o)
    => (k () -> IO ())
    -> (IO () -> IO ())
    -> Morph IO IO
    -> Morph m o
    -> Morph o m
    -> (IO () -> n ())
    -> Widget n m k
    -> o SWidget
runWidget nio post' post liftO liftOBack liftION = toWidget
 where
    liftIO' :: MonadIO l => IO a -> l a
    liftIO' = liftIO . post

    -- type Receive n m k a = (Command -> n ()) -> m (a -> k ())
    reg_ :: Receive n m k a -> Receive IO m IO a
    reg_ s f = liftM (nio .) $ s $ liftION . liftIO' . f

    reg :: Receive n m k a -> ((a -> IO ()) -> IO (Command -> IO ())) -> o (Command -> IO ())
    reg s f = do
        rer <- liftIO newEmptyMVar
        u <- liftIO $ f $ \x -> do
            re <- readMVar rer
            re x
        re <- liftO $ reg_ s u
        liftIO $ putMVar rer re
        return u

    ger :: (Command -> IO ()) -> Send n m a -> Send IO o a
    ger hd s f = liftO $ s $ \a -> liftION $ liftIO' $ do
        hd Block
        f a
        hd Unblock

    nhd :: Command -> IO ()
    nhd = const $ return ()

    toWidget :: Widget n m k -> o SWidget
    toWidget i = case i of

        Action m -> liftO m >>= toWidget
        Label s -> do
            w <- liftIO' $ labelNew Nothing
            ger nhd s $ labelSetLabel w
            return' w

        Canvas w h sc_ me r diaFun -> do

          cur <- liftIO $ newMVar Nothing
          cur' <- liftIO $ newMVar Nothing
          v <- liftIO newEmptyMVar

          (canvasDraw, canvas, af, dims) <- liftIO' $ do
            canvas <- drawingAreaNew
            widgetAddEvents canvas [PointerMotionMask]
            af <- aspectFrameNew 0.5 0.5 (Just $ fromIntegral w / fromIntegral h)
            canvas `onSizeRequest` return (Requisition w h)
            containerAdd af canvas
            let
              dims = do
                win <- widgetGetDrawWindow canvas
                (w, h) <- drawableGetSize win
                let (w', h') = (fromIntegral w, fromIntegral h)
                let sc = w' / sc_
                return (sc, w', h', w, h)

              tr sc w h dia = translate (r2 (w/2, h/2)) $ dia # scaleY (-1) # scale sc `atop` rect w h # fc white # lw 0

              draw dia_ = do
                swapMVar cur $ Just dia_
                let dia = freeze $ clearValue dia_
                (sc, w, h, wi, he) <- dims
                win <- widgetGetDrawWindow canvas
                drawWindowBeginPaintRect win $ Rectangle 0 0 wi he
                renderWithDrawable win $ snd $ renderDia Cairo (CairoOptions "" (Width w) RenderOnly True) $ tr sc w h dia
                drawWindowEndPaint win

            return (draw, canvas, af, dims)

          let -- compCoords :: (Double, Double) -> IO (MousePos a)
              compCoords (x,y) = do
                (sc, w, h, _, _) <- dims
                d <- readMVar cur
                let p = ((x - w / 2) / sc, (h / 2 - y) / sc)
                return $ MousePos p $ maybe mempty (`sample` p2 p) d

          _ <- reg me $ \re -> do
              on' canvas buttonPressEvent $ tryEvent $ do
--                click <- eventClick
                p <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re $ Click p
              on' canvas buttonReleaseEvent $ tryEvent $ do
--                click <- eventClick
                p <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re $ Release p
              on' canvas enterNotifyEvent $ tryEvent $ do
                p <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re $ MouseEnter p
              on' canvas leaveNotifyEvent $ tryEvent $ do
                p <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re $ MouseLeave p
              on' canvas motionNotifyEvent $ tryEvent $ do
                p <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re $ MoveTo p
              on' canvas scrollEvent $ tryEvent $ do
                p <- eventCoordinates >>= liftIO . compCoords
                dir <- eventScrollDirection
                liftIO $ re $ ScrollTo dir p
              on' canvas keyPressEvent $ tryEvent $ do
--                p <- eventCoordinates >>= liftIO . compCoords
                m <- eventModifier
                c <- eventKeyVal
                liftIO $ re $ KeyPress m c
          _ <- liftIO $ on canvas exposeEvent $ tryEvent $ liftIO $ do
                d <- readMVar cur'
                case d of
                    Just x -> putMVar v x
                    _ -> return ()

          canvasDraw' <- liftIO $ do
            v2 <- newMVar False
            forkIO $ do
              threadDelay 200000
              forever $ do
                threadDelay 10000
                dia <- takeMVar v
                swapMVar cur' $ Just dia
                swapMVar v2 True
                let d = diaFun dia
                post $ canvasDraw d
                swapMVar v2 False
                return ()
            return $ \dia -> do
                b <- readMVar v2
                unless b $ do
                    _ <- tryTakeMVar v
                    putMVar v dia

          ger nhd r canvasDraw'
          return' af

        Button s sens col m -> do
            w <- liftIO' buttonNew
            hd <- reg m $ \re -> on' w buttonActivated $ re ()
            ger hd s $ buttonSetLabel w
            ger hd sens $ widgetSetSensitive w
            ger hd col $ \c -> do
                widgetModifyBg w StateNormal c
                widgetModifyBg w StatePrelight c
            return' w
        Entry (r, s) -> do
            w <- liftIO' entryNew
            hd <- reg s $ \re -> on' w entryActivate $ entryGetText w >>= re
            hd' <- reg s $ \re -> on' w focusOutEvent $ lift $ entryGetText w >>= re >> return False
            ger (\x -> hd x >> hd' x) r $ entrySetText w
            return' w
        Checkbox (r, s) -> do
            w <- liftIO' checkButtonNew
            hd <- reg s $ \re -> on' w toggled $ toggleButtonGetActive w >>= re
            ger hd r $ toggleButtonSetActive w
            return' w
        Scale a b c (r, s) -> do
            w <- liftIO' $ hScaleNewWithRange a b c
            liftIO' $ w `onSizeRequest` return (Requisition 200 40)
            hd <- reg s $ \re -> on' w valueChanged $ rangeGetValue w >>= re
            ger hd r $ rangeSetValue w
            return' w
        Combobox ss (r, s) -> do
            w <- liftIO' comboBoxNewText
            liftIO' $ w `onSizeRequest` return (Requisition 50 30)
            liftIO' $ flip mapM_ ss $ comboBoxAppendText w
            hd <- reg s $ \re -> on' w changed $ fmap (max 0) (comboBoxGetActive w) >>= re
            ger hd r $ comboBoxSetActive w
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
            _ <- reg s $ \re -> on' w switchPage $ re
            return'' ws w
        Cell onCh f -> do
            let b = False
            w <- liftIO' $ case b of
                True -> fmap castToContainer $ hBoxNew False 1
                False -> fmap castToContainer $ alignmentNew 0 0 1 1
            sh <- liftIO $ newMVar $ return ()
            liftO $ onCh $ \bv -> do
                mx <- f (liftOBack . toWidget) bv
                return $ mx >>= \(x, y) -> liftOBack $ liftIO' $ do 
                    _ <- swapMVar sh x
                    containerForeach w $ if b then widgetHideAll else containerRemove w 
                    post' $ post $ do
                        ch <- containerGetChildren w
                        when (y `notElem` ch) $ containerAdd w y
                        x
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

mapFst f (a, b) = (f a, b)

containerAdd'' w x = do
    a <- alignmentNew 0 0 0 0
    containerAdd a x
    containerAdd w a
    set w [ boxChildPacking a := PackNatural ]
    return $ widgetShow a


