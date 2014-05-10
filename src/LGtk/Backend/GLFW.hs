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
import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
--import Control.Lens hiding ((#))
import Data.Vector.Storable (unsafeWith, unsafeFromForeignPtr0)
import Foreign
import System.IO
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.GLFW hiding (Key (..), ModifierKeys (..))
import qualified Graphics.UI.GLFW as GLFW

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

runWidget :: (forall m . (EffIORef m, MonadFix m) => Widget m) -> IO ()
runWidget desc = do
    hSetBuffering stdout NoBuffering

    (widget, actions) <- runPure newChan' $ unWrap $ runWidget_ $ inCanvas 800 600 30 desc
    _ <- forkIO actions

    case widget of
      SWidget width height sc_ handle keyhandle current iodia -> do

        _ <- GLFW.init
--        print =<< getVersion
        Just win <- createWindow width height "Diagrams + GLFW" Nothing Nothing
        makeContextCurrent (Just win) -- for OpenGL

        exit <- newMVar False
        postedActions <- newMVar $ return ()
        mc <- newMVar (0, Nothing)
        current' <- newMVar mempty

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
                d <- readMVar current'
                let p = p2 ((x - w / 2) / sc, (h / 2 - y) / sc)
                    q = MousePos p $ d `sample` p
                return (q, d)

            logMousePos :: CursorPosCallback
            logMousePos _win x y = do
                t <- modifyMVar mc $ \(tick, _) -> return ((tick+1, Just (x,y)), tick+1)
                post $ do
                    (t',q) <- readMVar mc
                    case q of
                        Just q | t==t' -> do
                            calcMousePos q >>= \(q,d) -> handle (MoveTo q, d)
                        _ -> return ()

            logMouseButton :: MouseButtonCallback
            logMouseButton _win _button state _mod = post $ do
                --putStrLn $ "MouseButtonCallback: " ++ show (button,state,mod)
                (_, p) <- readMVar mc
                case (state, p) of
                  (MouseButtonState'Pressed, Just p) -> calcMousePos p >>= \(q,d) -> handle (Click q, d)
                  (MouseButtonState'Released, Just p) -> calcMousePos p >>= \(q,d) -> handle (Release q, d)
                  _ -> return ()

            logKey :: KeyCallback
            logKey _win key _scancode action mods = do
                when (key == GLFW.Key'Escape) $ swapMVar exit True >> return ()
    --                putStrLn $ "KeyCallback: " ++ show (action, key,mods)
                post $ when (action `elem` [KeyState'Pressed, KeyState'Repeating]) $ keyhandle $ trKey mods key

            logWinSize :: WindowSizeCallback
            logWinSize _win _w _h = do
                _ <- tryTakeMVar iodia
                current >>= putMVar iodia

        -- callbacks
        setKeyCallback win (Just logKey)
        setMouseButtonCallback win (Just logMouseButton)
        setCursorPosCallback win (Just logMousePos)
        setWindowSizeCallback win (Just logWinSize)
        setWindowCloseCallback win $ Just $ \_ -> swapMVar exit True >> return ()

        let redraw = do
            dia_ <- tryTakeMVar iodia
            case dia_ of
              Nothing -> return ()
              Just dia_ -> do
                (sc, w, h, sw, sh) <- dims
                let dia = dia_ # clearValue # freeze # scale sc # clipped (rect w h) <>
                            rect w h # fc white # lw 0

                -- Cairo
                image <- imageRGBA8FromUnsafePtr sw sh <$> renderForeignPtrOpaque sw sh dia

                -- Rasterific
                --let sizeSpec = mkSizeSpec (Just $ fromIntegral w) (Just $ fromIntegral h)
                --let image = renderDia Rasterific (RasterificOptions sizeSpec) dia

                copyToScreen win (fromIntegral sw) (fromIntegral sh) image
--                putStr "*"

                _ <- swapMVar current' dia_
                return ()

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


trKey :: GLFW.ModifierKeys -> GLFW.Key -> ModifiedKey
trKey (GLFW.ModifierKeys s c a sup) k = ModifiedKey s c a sup $ case k of
    GLFW.Key'Space -> Key'Char ' '
    GLFW.Key'Apostrophe -> Key'Char '\''
    GLFW.Key'Comma -> Key'Char ','
    GLFW.Key'Minus -> Key'Char '-'
    GLFW.Key'Period -> Key'Char '.'
    GLFW.Key'Slash -> Key'Char '/'
    GLFW.Key'0 -> Key'Char '0'
    GLFW.Key'1 -> Key'Char '1'
    GLFW.Key'2 -> Key'Char '2'
    GLFW.Key'3 -> Key'Char '3'
    GLFW.Key'4 -> Key'Char '4'
    GLFW.Key'5 -> Key'Char '5'
    GLFW.Key'6 -> Key'Char '6'
    GLFW.Key'7 -> Key'Char '7'
    GLFW.Key'8 -> Key'Char '8'
    GLFW.Key'9 -> Key'Char '9'
    GLFW.Key'Semicolon -> Key'Char ';'
    GLFW.Key'Equal -> Key'Char '='
    GLFW.Key'A -> key s 'a'
    GLFW.Key'B -> key s 'b'
    GLFW.Key'C -> key s 'c'
    GLFW.Key'D -> key s 'd'
    GLFW.Key'E -> key s 'e'
    GLFW.Key'F -> key s 'f'
    GLFW.Key'G -> key s 'g'
    GLFW.Key'H -> key s 'h'
    GLFW.Key'I -> key s 'i'
    GLFW.Key'J -> key s 'j'
    GLFW.Key'K -> key s 'k'
    GLFW.Key'L -> key s 'l'
    GLFW.Key'M -> key s 'm'
    GLFW.Key'N -> key s 'n'
    GLFW.Key'O -> key s 'o'
    GLFW.Key'P -> key s 'p'
    GLFW.Key'Q -> key s 'q'
    GLFW.Key'R -> key s 'r'
    GLFW.Key'S -> key s 's'
    GLFW.Key'T -> key s 't'
    GLFW.Key'U -> key s 'u'
    GLFW.Key'V -> key s 'v'
    GLFW.Key'W -> key s 'w'
    GLFW.Key'X -> key s 'x'
    GLFW.Key'Y -> key s 'y'
    GLFW.Key'Z -> key s 'z'
    GLFW.Key'LeftBracket -> Key'Char '['
    GLFW.Key'Backslash -> Key'Char '\\'
    GLFW.Key'RightBracket -> Key'Char ']'
--    GLFW.Key'GraveAccent -> Key'
--    GLFW.Key'World1 -> Key'
--    GLFW.Key'World2 -> Key'
    GLFW.Key'Escape -> Key'Escape
    GLFW.Key'Enter -> Key'Char '\n'
    GLFW.Key'Tab -> Key'Char '\t'
    GLFW.Key'Backspace -> Key'Backspace
    GLFW.Key'Insert -> Key'Insert
    GLFW.Key'Delete -> Key'Delete
    GLFW.Key'Right -> Key'Right
    GLFW.Key'Left -> Key'Left
    GLFW.Key'Down -> Key'Down
    GLFW.Key'Up -> Key'Up
    GLFW.Key'PageUp -> Key'PageUp
    GLFW.Key'PageDown -> Key'PageDown
    GLFW.Key'Home -> Key'Home
    GLFW.Key'End -> Key'End
{-
    GLFW.Key'CapsLock -> Key'
    GLFW.Key'ScrollLock -> Key'
    GLFW.Key'NumLock -> Key'
    GLFW.Key'PrintScreen -> Key'
    GLFW.Key'Pause -> Key'
    GLFW.Key'F1 -> Key'
    GLFW.Key'F2 -> Key'
    GLFW.Key'F3 -> Key'
    GLFW.Key'F4 -> Key'
    GLFW.Key'F5 -> Key'
    GLFW.Key'F6 -> Key'
    GLFW.Key'F7 -> Key'
    GLFW.Key'F8 -> Key'
    GLFW.Key'F9 -> Key'
    GLFW.Key'F10 -> Key'
    GLFW.Key'F11 -> Key'
    GLFW.Key'F12 -> Key'
    GLFW.Key'F13 -> Key'
    GLFW.Key'F14 -> Key'
    GLFW.Key'F15 -> Key'
    GLFW.Key'F16 -> Key'
    GLFW.Key'F17 -> Key'
    GLFW.Key'F18 -> Key'
    GLFW.Key'F19 -> Key'
    GLFW.Key'F20 -> Key'
    GLFW.Key'F21 -> Key'
    GLFW.Key'F22 -> Key'
    GLFW.Key'F23 -> Key'
    GLFW.Key'F24 -> Key'
    GLFW.Key'F25 -> Key'
    GLFW.Key'Pad0 -> Key'
    GLFW.Key'Pad1 -> Key'
    GLFW.Key'Pad2 -> Key'
    GLFW.Key'Pad3 -> Key'
    GLFW.Key'Pad4 -> Key'
    GLFW.Key'Pad5 -> Key'
    GLFW.Key'Pad6 -> Key'
    GLFW.Key'Pad7 -> Key'
    GLFW.Key'Pad8 -> Key'
    GLFW.Key'Pad9 -> Key'
    GLFW.Key'PadDecimal -> Key'
    GLFW.Key'PadDivide -> Key'
    GLFW.Key'PadMultiply -> Key'
    GLFW.Key'PadSubtract -> Key'
    GLFW.Key'PadAdd -> Key'
    GLFW.Key'PadEnter -> Key'
    GLFW.Key'PadEqual -> Key'
    GLFW.Key'LeftShift -> Key'
    GLFW.Key'LeftControl -> Key'
    GLFW.Key'LeftAlt -> Key'
    GLFW.Key'LeftSuper -> Key'
    GLFW.Key'RightShift -> Key'
    GLFW.Key'RightControl -> Key'
    GLFW.Key'RightAlt -> Key'
    GLFW.Key'RightSuper -> Key'
    GLFW.Key'Menu -> Key'
-}
    _ -> Key'Unknown
  where
    key False c = Key'Char c
    key True c = Key'Char $ toUpper c

newChan' = do
    ch <- newChan
    return (readChan ch, writeChan ch)

data SWidget = forall a . (Monoid a, Semigroup a)
    => SWidget Int Int Double ((MouseEvent a, Dia a) -> IO ()) (ModifiedKey -> IO ()) (IO (Dia a)) (MVar (Dia a))


runWidget_
    :: forall m . (MonadRegister m, IO ~ EffectM m) => Widget m -> m SWidget
runWidget_  m = m >>= \i -> case i of
    Canvas w h sc_ me keyh r diaFun -> do
        rer <- liftIO' $ newMVar mempty
        rer' <- liftIO' $ newMVar mempty
        _ <- onChangeSimple r $ \b -> liftIO' $ do
            let d = diaFun b
            _ <- tryTakeMVar rer
            putMVar rer d
            _ <- swapMVar rer' d
            return ()

        handle <- registerCallback me $ const $ return ()
        keyhandle <- registerCallback (\key -> fromMaybe (\_ -> return False) keyh key >> return ()) $ const $ return ()
        return $ SWidget w h sc_ handle keyhandle (readMVar rer') rer


copyToScreen win w h (Image width height dat) = do
    makeContextCurrent (Just win)
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
        glClearColor 1 0 0 1
        glClear gl_COLOR_BUFFER_BIT
      else do
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






