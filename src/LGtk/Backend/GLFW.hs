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
--import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
--import Control.Lens hiding ((#))
import Foreign
import System.IO
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.GLFW hiding (Key (..), ModifierKeys (..))
import qualified Graphics.UI.GLFW as GLFW

import Diagrams.Prelude hiding (Image)

-- Rasterific
--import Diagrams.Backend.Rasterific

-- Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.Rendering.Cairo ( Format (..)
                                , formatStrideForWidth
                                , renderWith
                                , withImageSurfaceForData
                                )

import Data.LensRef.Class
--import Data.LensRef
import Data.LensRef.Default
import LGtk.Effects
import LGtk.Widgets
import LGtk.Render
import LGtk.Key

-------------------------------

runRegister' :: Wrap (Register IO) a -> IO (a, IO ())
runRegister' (Wrap m) = runRegister newChan' m

runWidget :: (forall m . (EffIORef m, MonadFix m) => Widget m) -> IO ()
runWidget desc = do
    hSetBuffering stdout NoBuffering

    (widget, actions) <- runRegister' $ runWidget_ $ inCanvas 800 600 30 desc
    _ <- forkIO actions

    case widget of
      SWidget width height sc_ handle keyhandle current iodia -> do

        _ <- GLFW.init
--        print =<< getVersion
        Just win <- createWindow width height "Diagrams + GLFW" Nothing Nothing
        makeContextCurrent (Just win) -- for OpenGL

        exit <- newMVar False
        postedActions <- newMVar $ pure ()
        mc <- newMVar (0, Nothing)
        current' <- newMVar mempty

        let
            post :: IO () -> IO ()
            post m = modifyMVar_ postedActions $ \n -> pure $ n >> m

            dims = do
                (w, h) <- getFramebufferSize win
                let (w', h') = (fromIntegral w, fromIntegral h)
                let sc = w' / sc_
                pure (sc, w', h', w, h)

            calcMousePos (x,y) = do
                (sc, w, h, _, _) <- dims
                d <- readMVar current'
                let p = p2 ((x - w / 2) / sc, (h / 2 - y) / sc)
                    q = MousePos p $ d `sample` p
                pure (q, d)

            logMousePos :: CursorPosCallback
            logMousePos _win x y = do
                t <- modifyMVar mc $ \(tick, _) -> pure ((tick+1, Just (x,y)), tick+1)
                post $ do
                    (t',q) <- readMVar mc
                    case q of
                        Just q | t==t' -> do
                            calcMousePos q >>= \(q,d) -> handle (MoveTo q, d)
                        _ -> pure ()

            logMouseButton :: MouseButtonCallback
            logMouseButton _win _button state _mod = post $ do
                --putStrLn $ "MouseButtonCallback: " ++ show (button,state,mod)
                (_, p) <- readMVar mc
                case (state, p) of
                  (MouseButtonState'Pressed, Just p) -> calcMousePos p >>= \(q,d) -> handle (Click q, d)
                  (MouseButtonState'Released, Just p) -> calcMousePos p >>= \(q,d) -> handle (Release q, d)
                  _ -> pure ()

            logKey :: KeyCallback
            logKey _win key _scancode action mods = do
                when (key == GLFW.Key'Escape) $ swapMVar exit True >> pure ()
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
        setWindowCloseCallback win $ Just $ \_ -> swapMVar exit True >> pure ()

        let redraw = do
            dia_ <- tryTakeMVar iodia
            case dia_ of
              Nothing -> pure ()
              Just dia_ -> do
                (sc, w, h, sw, sh) <- dims
                let dia = dia_ # clearValue # freeze # scale sc # clipped (rect w h) <>
                            rect w h # fc white # lw 0

                -- Rasterific
                --let sizeSpec = mkSizeSpec (Just $ fromIntegral w) (Just $ fromIntegral h)
                --let image = renderDia Rasterific (RasterificOptions sizeSpec) dia

                -- Cairo
                image <- createImage win sw sh dia
                copyImage (image,Rect 0 0 sw sh) (Screen win,Rect 0 sh sw 0)
                disposeImage image

                swapBuffers win
--                putStr "*"

                _ <- swapMVar current' dia_
                pure ()

        let eventCycle = do
                pollEvents
                b <- readMVar exit
                when (not b) $ do
                    join $ swapMVar postedActions $ pure ()
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
    pure (readChan ch, writeChan ch)

data SWidget = forall a . (Monoid a, Semigroup a)
    => SWidget Int Int Double ((MouseEvent a, Dia a) -> IO ()) (ModifiedKey -> IO ()) (IO (Dia a)) (MVar (Dia a))


runWidget_
    :: forall m . (MonadRegister m, IO ~ EffectM m) => Widget m -> m SWidget
runWidget_  m = m >>= \i -> case i of
    Canvas w h sc_ me keyh r diaFun -> do
        rer <- liftIO' $ newMVar mempty
        rer' <- liftIO' $ newMVar mempty
        _ <- onChange r $ \b -> liftIO' $ do
            let d = diaFun b
            _ <- tryTakeMVar rer
            putMVar rer d
            _ <- swapMVar rer' d
            pure ()

        post <- askPostpone
        let keyhandle key = post $ fromMaybe (\_ -> pure False) keyh key >> pure ()
        pure $ SWidget w h sc_ (post . me) keyhandle (readMVar rer') rer


-----------------------
-- backend drawing operations

data Rect = Rect !Int !Int !Int !Int -- x1, y1, x2, y2

data Image
    = Screen
        { imgGLContext          :: GLFW.Window
        }
    | Image
        { imgGLContext          :: GLFW.Window
        , imgGLTextureObj       :: GLuint
        , imgGLFramebufferObj   :: GLuint
        }

createImage :: GLFW.Window -> Int -> Int -> Dia Any -> IO Image -- width height
createImage win width height dia = do
    makeContextCurrent (Just win)

    fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
    glBindFramebuffer gl_DRAW_FRAMEBUFFER fbo

    tex <- alloca $! \pto -> glGenTextures 1 pto >> peek pto

    glBindTexture gl_TEXTURE_2D tex
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST

    -- render image with cairo backend
    let stride = formatStrideForWidth fmt width
        fmt    = FormatRGB24
        size   = stride * height
    allocaArray size $ \pixelData -> do
        let opt    = CairoOptions
                { _cairoSizeSpec     = Dims (fromIntegral width) (fromIntegral height)
                , _cairoOutputType   = RenderOnly
                , _cairoBypassAdjust = False
                , _cairoFileName     = ""
                }
            (_, r) = renderDia Cairo opt dia
        withImageSurfaceForData pixelData fmt width height stride (`renderWith` r)
        glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (fromIntegral width) (fromIntegral height) 0 (fromIntegral gl_BGRA) gl_UNSIGNED_BYTE pixelData

    glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0

    status <- glCheckFramebufferStatus gl_FRAMEBUFFER
    when (status /= gl_FRAMEBUFFER_COMPLETE) $
        putStrLn $ "incomplete framebuffer: " ++ show status
    pure $ Image
        { imgGLContext          = win
        , imgGLTextureObj       = tex
        , imgGLFramebufferObj   = fbo
        }

disposeImage :: Image -> IO ()
disposeImage img = do
    makeContextCurrent (Just $ imgGLContext img)
    Foreign.with (imgGLFramebufferObj img) $ glDeleteFramebuffers 1
    Foreign.with (imgGLTextureObj img) $ glDeleteTextures 1

copyImage :: (Image,Rect) -> (Image,Rect) -> IO ()
copyImage (srcImg,Rect srcX1 srcY1 srcX2 srcY2) (dstImg,Rect dstX1 dstY1 dstX2 dstY2)
    | imgGLContext srcImg /= imgGLContext dstImg =
        putStrLn "copyImage error: images are from different GL contexts"
    | otherwise = do
        makeContextCurrent (Just $ imgGLContext srcImg)
        glBindFramebuffer gl_READ_FRAMEBUFFER $ case srcImg of
            Screen {}   -> 0
            Image {}    -> imgGLFramebufferObj srcImg
        glBindFramebuffer gl_DRAW_FRAMEBUFFER $ case dstImg of
            Screen {}   -> 0
            Image {}    -> imgGLFramebufferObj dstImg
        let f = fromIntegral
        glBlitFramebuffer (f $ srcX1) (f $ srcY1) (f $ srcX2) (f $ srcY2)
                          (f $ dstX1) (f $ dstY1) (f $ dstX2) (f $ dstY2)
                          gl_COLOR_BUFFER_BIT gl_LINEAR
