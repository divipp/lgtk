{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Backend.Gtk
    ( runWidget
    ) where

import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Concurrent
import Data.Maybe
import Data.List hiding (union)
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (Widget, Release, RefWriterOf)
import qualified Graphics.UI.Gtk as Gtk

import Data.LensRef.Class
import Data.LensRef.Default
import LGtk.Effects
import LGtk.Key
import LGtk.Widgets

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

-------------------------

runRegister' :: IO () -> ((RefWriterOf (RefCreator IO) () -> IO ()) -> RefCreatorPost IO a) -> IO (a, IO ())
runRegister' pa m = do
    ch <- newChan
    a <- runRefCreator $ \f -> flip runReaderT (writeChan ch . f) $ m $ writeChan ch . f
    pure $ (,) a $ forever $ join $ pa >> readChan ch

{- |
Run a Gtk widget description.

The widget is shown in a window and the thread enters into the Gtk event cycle.
It leaves the event cycle when the window is closed.
-}
runWidget :: (forall m . (EffIORef m, MonadFix m) => Widget m) -> IO ()
runWidget desc = gtkContext $ \postGUISync -> do
    postActionsRef <- newMVar $ pure ()
    let addPostAction m = modifyMVar_ postActionsRef $ \n -> pure $ n >> m
        runPostActions = join $ modifyMVar postActionsRef $ \m -> pure (pure (), m)
    (widget, actions) <- runRegister' runPostActions $ \post_ -> do
        w <- runWidget_ post_ addPostAction postGUISync desc
        liftIO' runPostActions
        pure w
    _ <- forkIO $ actions
    pure widget

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
    _ <- window `on` deleteEvent $ liftIO mainQuit >> pure False
    widgetShowAll window
    mainGUI

type SWidget = (IO (), Gtk.Widget)

-- | Run an @IO@ parametrized interface description with Gtk backend
runWidget_
    :: forall m . (MonadRefCreator m, MonadBaseControl IO (EffectM m))
    => (RefWriterOf m () -> EffectM m ())
    -> (IO () -> IO ())
    -> (forall a . IO a -> IO a)
    -> Widget m
    -> m SWidget
runWidget_ post_ post' post = toWidget
 where
    liftIO'' :: IO a -> m a
    liftIO'' = liftIO' . post

    -- type Receive n m k a = (RegionStatusChange -> n ()) -> m (a -> k ())
    reg :: Receive m a -> ((a -> IO ()) -> IO (RegionStatusChange -> IO ())) -> m (RegionStatusChange -> IO ())
    reg s f = do
        u <- liftEffectM $ liftBaseWith $ \unr -> f $ \x -> do
            _ <- unr $ post_ $ s x
            pure ()
        onRegionStatusChange (liftIO_ . post . u $)
        pure u

    ger :: Eq a => (RegionStatusChange -> IO ()) -> RefReaderOf m a -> (a -> IO ()) -> m ()
    ger hd s f = fmap (const ()) $ onChangeEq s $ \a -> liftIO'' $ do
        hd Block
        f a
        hd Unblock

    nhd :: RegionStatusChange -> IO ()
    nhd = const $ pure ()

    toWidget :: Widget m -> m SWidget
    toWidget m = m >>= \i -> case i of

        Label s -> do
            w <- liftIO'' $ labelNew Nothing
            ger nhd s $ labelSetLabel w
            pure' w

        Canvas w h sc_ me keyh r diaFun -> mkCanvas me r diaFun where

         mkCanvas
            :: forall b da
            .  (Monoid da, Semigroup da, Eq b)
            => ((MouseEvent da, Dia da) -> RefWriterOf m ())
            -> RefReaderOf m b
            -> (b -> Dia da)
            -> m SWidget
         mkCanvas me r diaFun = do

          cur <- liftIO' $ newMVar Nothing
          cur' <- liftIO' $ newMVar Nothing
          v <- liftIO' newEmptyMVar

          (canvasDraw, canvas, af, dims, drawingAreaGetDrawWindow') <- liftIO'' $ do
            canvas <- drawingAreaNew
            widgetAddEvents canvas [PointerMotionMask, KeyPressMask]
            widgetSetCanFocus canvas True
            af <- aspectFrameNew 0.5 0.5 (Just $ fromIntegral w / fromIntegral h)
            _ <- canvas `onSizeRequest` pure (Requisition w h)
            _ <- containerAdd af canvas
            realized <- newMVar False
            _ <- onRealize canvas $ swapMVar realized True >> pure ()
--            _ <- afterRealize canvas $ swapMVar realized False >> pure ()
            let
              drawingAreaGetDrawWindow' = do
                b <- readMVar realized
                if b then catchJust select (fmap Just $ widgetGetDrawWindow canvas) (const $ pure Nothing)
                     else pure Nothing
                where
                    select :: SomeException -> Maybe ()
                    select s | "widgetGetDrawWindow" `isInfixOf` show s = Just ()
                    select _ = Nothing
                    

              dims win = do
                (w, h) <- drawableGetSize win
                let (w', h') = (fromIntegral w, fromIntegral h)
                let sc = w' / sc_
                pure (sc, w', h', w, h)

              tr sc w h dia = translate (r2 (w/2, h/2)) $ dia # scaleY (-1) # scale sc `atop` rect w h # fc white # lwL 0

              draw dia_ = do
                _ <- swapMVar cur $ Just dia_
                let dia = clearValue dia_
                win <- drawingAreaGetDrawWindow'
                case win of
                  Nothing -> pure ()
                  Just win -> do
                    (sc, w, h, wi, he) <- dims win
                    drawWindowBeginPaintRect win $ Rectangle 0 0 wi he
                    renderWithDrawable win $ snd $ renderDia Cairo (CairoOptions "" (Width w) RenderOnly True) $ tr sc w h dia
                    drawWindowEndPaint win

            pure (draw, canvas, af, dims, drawingAreaGetDrawWindow')

          let -- compCoords :: (Double, Double) -> IO (MousePos a)
              compCoords_ (x,y) = do
                win <- drawingAreaGetDrawWindow'
                case win of
                  Nothing -> pure Nothing
                  Just win -> do
                    (sc, w, h, _, _) <- dims win
                    d <- readMVar cur
                    let p = p2 ((x - w / 2) / sc, (h / 2 - y) / sc)
                    pure $ Just (MousePos p $ maybe mempty (`sample` p) d, fromMaybe mempty d)

          _ <- reg me $ \re -> do
              let compCoords :: MonadIO em
                        => em (Double, Double)
                        -> ((MousePos da, Dia da) -> (MouseEvent da, Dia da))
                        -> em ()
                  compCoords eventCoordinates f = do
                    c <- eventCoordinates
                    liftIO $ compCoords_ c >>= maybe (pure ()) (re . f)

              _ <- on' canvas buttonPressEvent $ tryEvent $ compCoords eventCoordinates $ \(p, d) -> (Click p, d)
              _ <- on' canvas buttonReleaseEvent $ tryEvent $ compCoords eventCoordinates $ \(p, d) -> (Release p, d)
              _ <- on' canvas enterNotifyEvent $ tryEvent $ compCoords eventCoordinates $ \(p, d) -> (MouseEnter p, d)
              _ <- on' canvas leaveNotifyEvent $ tryEvent $ compCoords eventCoordinates $ \(p, d) -> (MouseLeave p, d)
              _ <- on' canvas motionNotifyEvent $ tryEvent $ compCoords eventCoordinates $ \(p, d) -> (MoveTo p, d)
              on' canvas scrollEvent $ tryEvent $ do
                dir <- eventScrollDirection
                let tr _ = Horizontal -- TODO
                compCoords eventCoordinates $ \(p, d) -> (ScrollTo (tr dir) p, d)

          case keyh of
            Nothing -> pure ()
            Just keyh -> do
                _ <- reg (\k -> keyh k >> pure ()) $ \re ->
                  on' canvas keyPressEvent $ tryEvent $ do
                    m <- eventModifier
                    c <- eventKeyVal
                    kn <- lift $ keyvalName c
                    kc <- lift $ keyvalToChar c
                    liftIO $ re $ trKey m kn kc
                pure ()

          _ <- liftIO' $ on canvas exposeEvent $ tryEvent $ liftIO $ do
                d <- readMVar cur'
                case d of
                    Just x -> putMVar v x
                    _ -> pure ()

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
                pure ()
            pure $ \dia -> do
                b <- readMVar v2
                unless b $ do
                    _ <- tryTakeMVar v
                    putMVar v dia

          ger nhd r canvasDraw'
          pure' af

        Button s sens col m -> do
            w <- liftIO'' buttonNew
            hd <- reg m $ \re -> on' w buttonActivated $ re ()
            ger hd s $ buttonSetLabel w
            ger hd sens $ widgetSetSensitive w
            let tr col = case toSRGB col of RGB r g b -> Color (f r) (f g) (f b)
                f d = fromInteger $ min 65535 $ max 0 $ floor $ d * 65536
            case col of
                Nothing -> pure ()
                Just col -> ger hd col $ \c -> do
                    widgetModifyBg w StateNormal $ tr c
                    widgetModifyBg w StatePrelight $ tr c
            pure' w
        Entry _ (r, s) -> do        -- TODO: red background if not ok
            w <- liftIO'' entryNew
            hd <- reg s $ \re -> on' w entryActivate $ entryGetText w >>= re
            hd' <- reg s $ \re -> on' w focusOutEvent $ lift $ entryGetText w >>= re >> pure False
            ger (\x -> hd x >> hd' x) r $ entrySetText w
            pure' w
        Checkbox (r, s) -> do
            w <- liftIO'' checkButtonNew
            hd <- reg s $ \re -> on' w toggled $ toggleButtonGetActive w >>= re
            ger hd r $ toggleButtonSetActive w
            pure' w
        Scale a b c (r, s) -> do
            w <- liftIO'' $ hScaleNewWithRange a b c
            _ <- liftIO'' $ w `onSizeRequest` pure (Requisition 200 40)
            hd <- reg s $ \re -> on' w valueChanged $ rangeGetValue w >>= re
            ger hd r $ rangeSetValue w
            pure' w
        Combobox ss (r, s) -> do
            w <- liftIO'' comboBoxNewText
            _ <- liftIO'' $ w `onSizeRequest` pure (Requisition 100 30)
            liftIO'' $ flip mapM_ ss $ comboBoxAppendText w
            hd <- reg s $ \re -> on' w changed $ fmap (max 0) (comboBoxGetActive w) >>= re
            ger hd r $ comboBoxSetActive w
            pure' w
        List o xs -> do
            ws <- mapM toWidget xs
            w <- liftIO'' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            shs <- forM ws $ liftIO'' . containerAdd'' w . snd
            fmap (mapFst (sequence_ shs >>)) $ pure'' ws w
        Notebook' s xs -> do
            ws <- mapM (toWidget . snd) xs
            w <- liftIO'' notebookNew
            forM_ (zip ws xs) $ \(ww, (s, _)) -> do
                liftIO'' . flip (notebookAppendPage w) s $ snd $ ww
            _ <- reg s $ \re -> on' w switchPage $ re
            pure'' ws w
        Cell onCh f -> do
            let b = False
            w <- liftIO'' $ case b of
                True -> fmap castToContainer $ hBoxNew False 1
                False -> fmap castToContainer $ alignmentNew 0 0 1 1
            sh <- liftIO' $ newMVar $ pure ()
            _ <- onChangeMemo onCh $ \bv -> do
                mx <- f toWidget bv
                pure $ mx >>= \(x,y) -> liftIO'' $ do 
                    _ <- swapMVar sh x
                    containerForeach w $ if b then widgetHideAll else containerRemove w 
                    post' $ post $ do
                        ch <- containerGetChildren w
                        when (y `notElem` ch) $ containerAdd w y
                        x
            fmap (mapFst (join (readMVar sh) >>)) $ pure'' [] w

on' :: GObjectClass x => x -> Signal x c -> c -> IO (RegionStatusChange -> IO ())
on' o s c = fmap (flip f) $ on o s c where
    f Kill = signalDisconnect
    f Block = signalBlock
    f Unblock = signalUnblock

pure' :: (Monad m, Applicative m) => WidgetClass x => x -> m SWidget
pure' w = pure (widgetShowAll w, castToWidget w)

pure'' :: (Monad m, Applicative m) => WidgetClass x => [SWidget] -> x -> m SWidget
pure'' ws w = pure (mapM_ fst ws >> widgetShow w, castToWidget w)

mapFst f (a, b) = (f a, b)

containerAdd'' w x = do
    a <- alignmentNew 0 0 0 0
    containerAdd a x
    containerAdd w a
    set w [ boxChildPacking a := PackNatural ]
    pure $ widgetShow a

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
            "Tab" -> Key'Char '\t'
            _ -> Key'Unknown


