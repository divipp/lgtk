{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Backend.Gtk
    ( runWidget
    ) where

import Control.Category
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Concurrent
import Data.Maybe
import Data.List hiding (union)
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (Widget, Release)
import qualified Graphics.UI.Gtk as Gtk

import Data.LensRef
#ifdef __PURE__
import Data.LensRef.Pure
#else
import Data.LensRef.Fast
#endif
import LGtk.Effects
import LGtk.Widgets

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

-------------------------

runWidget :: (forall m . (EffIORef m, MonadFix m) => Widget m) -> IO ()
runWidget w = runWidget' (\pa -> runPure (newChan' pa) . unWrap) w
  where
    newChan' pa = do
        ch <- newChan
        return (pa >> readChan ch, writeChan ch)

{- |
Run a Gtk widget description.

The widget is shown in a window and the thread enters into the Gtk event cycle.
It leaves the event cycle when the window is closed.
-}
runWidget' :: (EffRef m, MonadBaseControl IO (EffectM m))
    => (forall a . IO () -> m a -> IO (a, IO ())) -> Widget m -> IO ()
runWidget' run desc = gtkContext $ \postGUISync -> do
    postActionsRef <- newMVar $ return ()
    let addPostAction m = modifyMVar_ postActionsRef $ \n -> return $ n >> m
        runPostActions = join $ modifyMVar postActionsRef $ \m -> return (return (), m)
    (widget, actions) <- run runPostActions $ do
        w <- runWidget_ addPostAction postGUISync desc
        liftIO' runPostActions
        return w
    _ <- forkIO $ actions
    return widget

gtkContext :: ((forall a . IO a -> IO a) -> IO SWidget) -> IO ()
gtkContext m = do
    _ <- unsafeInitGUIForThreadedRTS
    tid <- myThreadId
    let post :: forall a . IO a -> IO a
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
runWidget_
    :: forall m . (EffRef m, MonadBaseControl IO (EffectM m))
    => (IO () -> IO ())
    -> (forall a . IO a -> IO a)
    -> Widget m
    -> m SWidget
runWidget_ post' post = toWidget
 where
    liftIO'' :: IO a -> m a
    liftIO'' = liftIO' . post

    -- type Receive n m k a = (Command -> n ()) -> m (a -> k ())
    reg :: Receive m a -> ((a -> IO ()) -> IO (Command -> IO ())) -> m (Command -> IO ())
    reg s f = do
        rer <- liftIO' newEmptyMVar
        u <- liftEffectM $ liftBaseWith $ \unr -> f $ \x -> do
            re <- readMVar rer
            _ <- unr $ re x
            return ()
        re <- toReceive s $ \x -> liftIO_ . post . u $ x
        liftIO' $ putMVar rer re
        return u

    ger :: Eq a => (Command -> IO ()) -> ReadRef m a -> (a -> IO ()) -> m ()
    ger hd s f = liftM (const ()) $ rEffect s $ \a -> liftBaseWith . const $ post $ do
        hd Block
        f a
        hd Unblock

    nhd :: Command -> IO ()
    nhd = const $ return ()

    toWidget :: Widget m -> m SWidget
    toWidget m = m >>= \i -> case i of

        Label s -> do
            w <- liftIO'' $ labelNew Nothing
            ger nhd s $ labelSetLabel w
            return' w

        Canvas w h sc_ me keyh r diaFun -> do

          cur <- liftIO' $ newMVar Nothing
          cur' <- liftIO' $ newMVar Nothing
          v <- liftIO' newEmptyMVar

          (canvasDraw, canvas, af, dims) <- liftIO'' $ do
            canvas <- drawingAreaNew
            widgetAddEvents canvas [PointerMotionMask, KeyPressMask]
            widgetSetCanFocus canvas True
            af <- aspectFrameNew 0.5 0.5 (Just $ fromIntegral w / fromIntegral h)
            _ <- canvas `onSizeRequest` return (Requisition w h)
            _ <- containerAdd af canvas
            let
              dims = do
                win <- widgetGetDrawWindow canvas
                (w, h) <- drawableGetSize win
                let (w', h') = (fromIntegral w, fromIntegral h)
                let sc = w' / sc_
                return (sc, w', h', w, h)

              tr sc w h dia = translate (r2 (w/2, h/2)) $ dia # scaleY (-1) # scale sc `atop` rect w h # fc white # lw 0

              draw dia_ = do
                _ <- swapMVar cur $ Just dia_
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
                return (MousePos p $ maybe mempty (`sample` p2 p) d, fromMaybe mempty d)

          _ <- reg me $ \re -> do
              _ <- on' canvas buttonPressEvent $ tryEvent $ do
--                click <- eventClick
                (p, d) <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re (Click p, d)
              _ <- on' canvas buttonReleaseEvent $ tryEvent $ do
--                click <- eventClick
                (p, d) <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re (Release p, d)
              _ <- on' canvas enterNotifyEvent $ tryEvent $ do
                (p, d) <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re (MouseEnter p, d)
              _ <- on' canvas leaveNotifyEvent $ tryEvent $ do
                (p, d) <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re (MouseLeave p, d)
              _ <- on' canvas motionNotifyEvent $ tryEvent $ do
                (p, d) <- eventCoordinates >>= liftIO . compCoords
                liftIO $ re (MoveTo p, d)
              on' canvas scrollEvent $ tryEvent $ do
                (p, d) <- eventCoordinates >>= liftIO . compCoords
                dir <- eventScrollDirection
                let tr _ = Horizontal -- TODO
                liftIO $ re (ScrollTo (tr dir) p, d)

          case keyh of
            Nothing -> return ()
            Just keyh -> do
                _ <- reg (\k -> keyh k >> return ()) $ \re ->
                  on' canvas keyPressEvent $ tryEvent $ do
    --                p <- eventCoordinates >>= liftIO . compCoords
                    m <- eventModifier
                    c <- eventKeyVal
                    kn <- lift $ keyvalName c
                    kc <- lift $ keyvalToChar c
                    liftIO $ re $ trKey m kn kc
                return ()

          _ <- liftIO' $ on canvas exposeEvent $ tryEvent $ liftIO $ do
                d <- readMVar cur'
                case d of
                    Just x -> putMVar v x
                    _ -> return ()

          canvasDraw' <- liftIO' $ do
            v2 <- newMVar False
            _ <- forkIO $ do
              threadDelay 200000
              forever $ do
                threadDelay 10000
                dia <- takeMVar v
                _ <- swapMVar cur' $ Just dia
                _ <- swapMVar v2 True
                let d = diaFun dia
                post $ canvasDraw d
                _ <- swapMVar v2 False
                return ()
            return $ \dia -> do
                b <- readMVar v2
                unless b $ do
                    _ <- tryTakeMVar v
                    putMVar v dia

          ger nhd r canvasDraw'
          return' af

        Button s sens col m -> do
            w <- liftIO'' buttonNew
            hd <- reg m $ \re -> on' w buttonActivated $ re ()
            ger hd s $ buttonSetLabel w
            ger hd sens $ widgetSetSensitive w
            let tr col = case toSRGB col of RGB r g b -> Color (f r) (f g) (f b)
                f d = fromInteger $ min 65535 $ max 0 $ floor $ d * 65536
            case col of
                Nothing -> return ()
                Just col -> ger hd col $ \c -> do
                    widgetModifyBg w StateNormal $ tr c
                    widgetModifyBg w StatePrelight $ tr c
            return' w
        Entry _ (r, s) -> do        -- TODO: red background if not ok
            w <- liftIO'' entryNew
            hd <- reg s $ \re -> on' w entryActivate $ entryGetText w >>= re
            hd' <- reg s $ \re -> on' w focusOutEvent $ lift $ entryGetText w >>= re >> return False
            ger (\x -> hd x >> hd' x) r $ entrySetText w
            return' w
        Checkbox (r, s) -> do
            w <- liftIO'' checkButtonNew
            hd <- reg s $ \re -> on' w toggled $ toggleButtonGetActive w >>= re
            ger hd r $ toggleButtonSetActive w
            return' w
        Scale a b c (r, s) -> do
            w <- liftIO'' $ hScaleNewWithRange a b c
            _ <- liftIO'' $ w `onSizeRequest` return (Requisition 200 40)
            hd <- reg s $ \re -> on' w valueChanged $ rangeGetValue w >>= re
            ger hd r $ rangeSetValue w
            return' w
        Combobox ss (r, s) -> do
            w <- liftIO'' comboBoxNewText
            _ <- liftIO'' $ w `onSizeRequest` return (Requisition 50 30)
            liftIO'' $ flip mapM_ ss $ comboBoxAppendText w
            hd <- reg s $ \re -> on' w changed $ fmap (max 0) (comboBoxGetActive w) >>= re
            ger hd r $ comboBoxSetActive w
            return' w
        List o xs -> do
            ws <- mapM toWidget xs
            w <- liftIO'' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            shs <- forM ws $ liftIO'' . containerAdd'' w . snd
            liftM (mapFst (sequence_ shs >>)) $ return'' ws w
        Notebook' s xs -> do
            ws <- mapM (toWidget . snd) xs
            w <- liftIO'' notebookNew
            forM_ (zip ws xs) $ \(ww, (s, _)) -> do
                liftIO'' . flip (notebookAppendPage w) s $ snd $ ww
            _ <- reg s $ \re -> on' w switchPage $ re
            return'' ws w
        Cell onCh f -> do
            let b = False
            w <- liftIO'' $ case b of
                True -> fmap castToContainer $ hBoxNew False 1
                False -> fmap castToContainer $ alignmentNew 0 0 1 1
            sh <- liftIO' $ newMVar $ return ()
            _ <- onChange onCh $ \bv -> do
                mx <- f toWidget bv
                return $ mx >>= \(x,y) -> liftIO'' $ do 
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

trKey mods name ch
    = ModifiedKey (Gtk.Shift `elem` mods) (Gtk.Control `elem` mods) (Gtk.Alt `elem` mods) (Gtk.Super `elem` mods) k
  where
    k = case ch of
        Just c -> Key'Char c
        _ -> case name of
            "Escape" -> Key'Escape
            "BackSpace" -> Key'Backspace
            "Insert" -> Key'Insert
            "Delete" -> Key'Delete
            "Right" -> Key'Right
            "Left" -> Key'Left
            "Down" -> Key'Down
            "Up" -> Key'Up
            "PageUp" -> Key'PageUp
            "PageDown" -> Key'PageDown
            "Home" -> Key'Home
            "End" -> Key'End
            "Return" -> Key'Char '\n'
            _ -> Key'Unknown


