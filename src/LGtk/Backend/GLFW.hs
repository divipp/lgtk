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
import Data.IORef
import Data.Vector.Storable (unsafeWith, unsafeFromForeignPtr0)
import Foreign
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
import Data.LensRef.Fast
import LGtk.Effects
import LGtk.Widgets

-------------------------------

runWidget :: (forall m . (EffIORef m) => Widget m) -> IO ()
runWidget desc = do

    exitRef <- newIORef False
    _ <- GLFW.init
    print =<< getVersion
    let width = 600
        height = 400
    Just w <- createWindow width height "Diagrams + Rasterific + GLFW" Nothing Nothing

    makeContextCurrent (Just w) -- for OpenGL

    let render dia = do
            (sw,sh) <- getFramebufferSize w
            (w,h) <- return (sw,sh)

            -- Cairo
            image <- imageRGBA8FromUnsafePtr w h <$> renderForeignPtrOpaque w h dia

            -- Rasterific
            --let sizeSpec = mkSizeSpec (Just $ fromIntegral w) (Just $ fromIntegral h)
            --let image = renderDia Rasterific (RasterificOptions sizeSpec) dia

            copyToScreen (fromIntegral sw) (fromIntegral sh) image
            putStrLn "render"


    (widget, actions) <- runPure newChan' $ unWrap $ do
        runWidget_ liftIO' desc
    _ <- forkIO $ actions

    case widget of
      SWidget _wi _he sc_ handle current iodia -> do

        mc <- newMVar (0,0)
        let
            dims = do
                (w, h) <- getFramebufferSize w
                let (w', h') = (fromIntegral w, fromIntegral h)
                let sc = w' / sc_
                return (sc, w', h', w, h)


            compCoords (x,y) = do
                (sc, w, h, _, _) <- dims
                d <- current
                let p = ((x - w / 2) / sc, (h / 2 - y) / sc)
--                print p
                return $ MousePos p $ d `sample` p2 p

            logMousePos :: CursorPosCallback
            logMousePos _win x y = do
                _ <- swapMVar mc (x,y)
                p <- compCoords (x,y)
                handle $ MoveTo p

            logMouseButton :: MouseButtonCallback
            logMouseButton _win _button state _mod = do
                --putStrLn $ "MouseButtonCallback: " ++ show (button,state,mod)
                (x,y) <- readMVar mc
                p <- compCoords (x,y)
                handle $ case state of
                  MouseButtonState'Pressed -> Click p
                  MouseButtonState'Released -> Release p

            logKey :: IORef Bool -> KeyCallback
            logKey ref _win key _scancode action mods = do
                writeIORef ref $ key == Key'Escape
                case action of
                    KeyState'Pressed -> do
--                        putStrLn $ "KeyCallback: " ++ show (key,mods)
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
                    _ -> return ()

            logChar :: CharCallback
            logChar _win _char = return () --putStrLn $ "CharCallback: " ++ show (char)

            logWinSize :: WindowSizeCallback
            logWinSize _win w h = putStrLn $ "WindowSizeCallback: " ++ show (w,h)

        -- callbacks
        setKeyCallback w (Just $ logKey exitRef)
        setCharCallback w (Just logChar)
        setMouseButtonCallback w (Just logMouseButton)
        setCursorPosCallback w (Just logMousePos)
        setWindowSizeCallback w (Just logWinSize)

        let
            tr sc w h dia = {-translate (r2 (w/2, h/2)) $ -} dia # {-scaleY (-1) # -} scale sc # clipped (rect w h) <>
                                                             rect w h # fc white # lw 0

            run = do
                dia_ <- iodia
                let dia = freeze $ clearValue dia_
                (sc, w', h, _, _) <- dims
                render $ tr sc w' h dia
                swapBuffers w

            run' = do
                waitEvents
                --pollEvents
                exit <- readIORef exitRef
                unless exit run'

        _ <- forkIO $ forever run

        run'
        destroyWindow w
        terminate


newChan' = do
    ch <- newChan
    return (readChan ch, writeChan ch)

data SWidget = forall a . (Monoid a, Semigroup a) => SWidget Int Int Double (MouseEvent a -> IO ()) (IO (Dia a)) (IO (Dia a))


runWidget_
    :: forall m . (EffRef m, IO ~ EffectM m)
    => (forall a . IO a -> m a)
    -> Widget m
    -> m SWidget
runWidget_ liftIO_ = toWidget
 where
    toWidget :: Widget m -> m SWidget
    toWidget m = m >>= \i -> case i of

        Canvas w h sc_ me r diaFun -> do
            rer <- liftIO_ $ newMVar mempty
            rer' <- liftIO_ $ newMVar mempty
            _ <- onChange r $ \b -> return $ do
                liftIO_ $ do
                    let d = diaFun b
                    _ <- tryTakeMVar rer
                    putMVar rer d
                    _ <- swapMVar rer' d
                    return d

            let iodia = takeMVar rer
            let current = readMVar rer'
            handle <- toReceive me (const $ return ())
            return $ SWidget w h sc_ handle current iodia


copyToScreen w h (Image width height dat) = do
    let iw = fromIntegral width
        ih = fromIntegral height
    fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
    glBindFramebuffer gl_DRAW_FRAMEBUFFER fbo

    tex <- alloca $! \pto -> glGenTextures 1 pto >> peek pto

    glBindTexture gl_TEXTURE_2D tex
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    unsafeWith dat $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) iw ih 0 (fromIntegral gl_RGBA) gl_UNSIGNED_BYTE
    glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0

    status <- glCheckFramebufferStatus gl_FRAMEBUFFER
    unless (status == gl_FRAMEBUFFER_COMPLETE) $
        putStrLn $ "incomplete framebuffer: " ++ show status

    glBindFramebuffer gl_DRAW_FRAMEBUFFER 0

    glBindFramebuffer gl_READ_FRAMEBUFFER fbo
    glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
    glBlitFramebuffer 0 ih iw 0 0 0 w h gl_COLOR_BUFFER_BIT gl_LINEAR
    glBindFramebuffer gl_READ_FRAMEBUFFER 0
    glBindFramebuffer gl_DRAW_FRAMEBUFFER 0

    Foreign.with fbo $ glDeleteFramebuffers 1
    Foreign.with tex $ glDeleteTextures 1

imageRGBA8FromUnsafePtr :: Int -> Int -> ForeignPtr Word8 -> Image PixelRGBA8
imageRGBA8FromUnsafePtr w h ptr = Image w h $ unsafeFromForeignPtr0 ptr (w * h * 4)


