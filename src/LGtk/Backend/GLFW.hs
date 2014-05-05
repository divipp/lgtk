{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Backend.GLFW
    ( runWidget
    ) where

import Data.Char
import Control.Applicative
import Control.Concurrent
import Control.Monad
--import Control.Lens hiding ((#))
import Data.Vector.Storable (unsafeWith, unsafeFromForeignPtr0)
import Foreign
import System.IO
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.GLFW as GLFW

import Diagrams.Prelude hiding (Image)

-- Rasterific
--import Diagrams.Backend.Rasterific

-- Cairo
--import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Ptr

import Codec.Picture

import Data.LensRef
#ifdef __PURE__
import Data.LensRef.Pure
#else
import Data.LensRef.Fast
#endif
import LGtk.Effects
import LGtk.Widgets
import LGtk.Render

-------------------------------

runWidget :: (forall m . (EffIORef m) => Widget m) -> IO ()
runWidget desc = do
    hSetBuffering stdout NoBuffering

    (widget, actions) <- runPure newChan' $ unWrap $ runWidget_ $ inCanvas 800 600 30 desc
    _ <- forkIO actions

    case widget of
      SWidget width height sc_ handle current iodia -> do

        _ <- GLFW.init
        print =<< getVersion
        Just win <- createWindow width height "Diagrams + Rasterific + GLFW" Nothing Nothing
        makeContextCurrent (Just win) -- for OpenGL

        exit <- newMVar False
        postedActions <- newMVar $ return ()
        mc <- newMVar (0, Nothing)

        let
            post :: IO () -> IO ()
            post m = modifyMVar_ postedActions $ \n -> return $ n >> m

            dims = do
                (w, h) <- getFramebufferSize win
                let (w', h') = (fromIntegral w, fromIntegral h)
                let sc = w' / sc_
                return (sc, w', h', w, h)

            calcMousePos (x,y) = do
                (sc, w, h, _, _) <- dims
                d <- current
                let p = ((x - w / 2) / sc, (h / 2 - y) / sc)
                    q = MousePos p $ d `sample` p2 p
                return q

            logMousePos :: CursorPosCallback
            logMousePos _win x y = do
                t <- modifyMVar mc $ \(tick, _) -> return ((tick+1, Just (x,y)), tick+1)
                post $ do
                    (t',q) <- readMVar mc
                    case q of
                        Just q | t==t' -> do
                            calcMousePos q >>= handle . MoveTo
                        _ -> return ()

            logMouseButton :: MouseButtonCallback
            logMouseButton _win _button state _mod = post $ do
                --putStrLn $ "MouseButtonCallback: " ++ show (button,state,mod)
                (_, p) <- readMVar mc
                case (state, p) of
                  (MouseButtonState'Pressed, Just p) -> calcMousePos p >>= handle . Click
                  (MouseButtonState'Released, Just p) -> calcMousePos p >>= handle . Release
                  _ -> return ()

            logKey :: KeyCallback
            logKey _win key _scancode action mods = do
                when (key == Key'Escape) $ swapMVar exit True >> return ()
    --                putStrLn $ "KeyCallback: " ++ show (action, key,mods)
                post $ when (action `elem` [KeyState'Pressed, KeyState'Repeating]) $ do
                        let name = case key of
                                Key'Backspace -> "BackSpace"
                                Key'Delete -> "Delete"
                                Key'Tab -> "Tab"
                                Key'Left -> "Left"
                                Key'Right -> "Right"
                                _ -> ""
                        let char = case key of
                                Key'Space -> Just ' '
                                _ -> case show key of
                                    ['K','e','y','\'',c] -> Just $ (if modifierKeysShift mods then id else toLower) c
                                    _ -> Nothing
                        handle $ KeyPress [ControlModifier | modifierKeysControl mods] name char

            logWinSize :: WindowSizeCallback
            logWinSize _win _w _h = do
                _ <- tryTakeMVar iodia
                current >>= putMVar iodia . clearValue

        -- callbacks
        setKeyCallback win (Just logKey)
        setMouseButtonCallback win (Just logMouseButton)
        setCursorPosCallback win (Just logMousePos)
        setWindowSizeCallback win (Just logWinSize)

        let redraw = do
            dia_ <- tryTakeMVar iodia
            case dia_ of
              Nothing -> return ()
              Just dia_ -> do
                (sc, w, h, sw, sh) <- dims
                let dia = dia_ # freeze # scale sc # clipped (rect w h) <>
                            rect w h # fc white # lw 0

                -- Cairo
                image <- imageRGBA8FromUnsafePtr sw sh <$> renderForeignPtrOpaque sw sh dia

                -- Rasterific
                --let sizeSpec = mkSizeSpec (Just $ fromIntegral w) (Just $ fromIntegral h)
                --let image = renderDia Rasterific (RasterificOptions sizeSpec) dia

                copyToScreen win (fromIntegral sw) (fromIntegral sh) image
--                putStr "*"

        let eventCycle = do
                pollEvents
                b <- readMVar exit
                when (not b) $ do
                    join $ swapMVar postedActions $ return ()
                    redraw
                    threadDelay 10000
                    eventCycle

        eventCycle
        destroyWindow win
        terminate



newChan' = do
    ch <- newChan
    return (readChan ch, writeChan ch)

data SWidget = forall a . (Monoid a, Semigroup a) => SWidget Int Int Double (MouseEvent a -> IO ()) (IO (Dia a)) (MVar (Dia Any))


runWidget_
    :: forall m . (EffRef m, IO ~ EffectM m) => Widget m -> m SWidget
runWidget_  m = m >>= \i -> case i of
    Canvas w h sc_ me r diaFun -> do
        rer <- liftIO' $ newMVar mempty
        rer' <- liftIO' $ newMVar mempty
        _ <- onChangeSimple r $ \b -> liftIO' $ do
            let d = diaFun b
            _ <- tryTakeMVar rer
            putMVar rer $ d # clearValue
            _ <- swapMVar rer' d
            return ()

        handle <- toReceive me $ const $ return ()
        return $ SWidget w h sc_ handle (readMVar rer') rer


copyToScreen win w h (Image width height dat) = do
    let iw = fromIntegral width
        ih = fromIntegral height
    fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
    glBindFramebuffer gl_DRAW_FRAMEBUFFER fbo

    tex <- alloca $! \pto -> glGenTextures 1 pto >> peek pto

    glBindTexture gl_TEXTURE_2D tex
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    unsafeWith dat $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) iw ih 0 (fromIntegral gl_BGRA) gl_UNSIGNED_BYTE
    glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0

    status <- glCheckFramebufferStatus gl_FRAMEBUFFER
    if (status /= gl_FRAMEBUFFER_COMPLETE)
      then do
        putStrLn $ "incomplete framebuffer: " ++ show status
      else do
        glBindFramebuffer gl_DRAW_FRAMEBUFFER 0

        glBindFramebuffer gl_READ_FRAMEBUFFER fbo
        glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
        glBlitFramebuffer 0 ih iw 0 0 0 w h gl_COLOR_BUFFER_BIT gl_LINEAR
        glBindFramebuffer gl_READ_FRAMEBUFFER 0
        glBindFramebuffer gl_DRAW_FRAMEBUFFER 0

        Foreign.with fbo $ glDeleteFramebuffers 1
        Foreign.with tex $ glDeleteTextures 1

        swapBuffers win


imageRGBA8FromUnsafePtr :: Int -> Int -> ForeignPtr Word8 -> Image PixelRGBA8
imageRGBA8FromUnsafePtr w h ptr = Image w h $ unsafeFromForeignPtr0 ptr (w * h * 4)

-----------------------

{-
low level primitives

data Image = { width, height :: Int, Ptr }
data Point
data Rect = { corner1, corner2 :: Point }

1.
resizeCopy :: (Rect, Image) -> (Rect, Image) -> ReaderT x IO ()
2.
render :: (Rect, Dia) -> (Rect, Image) -> ReaderT x IO ()
3.
-- to full screen
toScreen :: (Rect, Image) -> ReaderT x IO ()


-}






