{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
module LGtk.Backend.GLFW
    ( Base
    , runWidget
    ) where

import Data.Char
import Data.Maybe
--import Control.Applicative
import Control.Concurrent
import Control.Monad
--import Control.Monad.Reader
--import Control.Monad.Fix
--import Control.Lens hiding ((#))
import Foreign
import System.IO
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.GLFW hiding (Key (..), ModifierKeys (..))
import qualified Graphics.UI.GLFW as GLFW

import Diagrams.Prelude

#ifdef __RASTERIFIC__
import Diagrams.Backend.Rasterific
import qualified Codec.Picture.Types as JP
import Data.Vector.Storable (unsafeWith)
#else
import Diagrams.Backend.Cairo.Internal
import Graphics.Rendering.Cairo ( Format (..)
                                , formatStrideForWidth
                                , renderWith
                                , withImageSurfaceForData
                                )
#endif

import Data.LensRef.Class
import Data.LensRef
import Data.LensRef.Default
import LGtk.Effects
import LGtk.Widgets
import LGtk.Render
import LGtk.Key

-------------------------------

type Base = IO

type RefCreator = RefCreatorPost Base

data SWidget = forall a . (Monoid a, Semigroup a)
    => SWidget Int Int Double ((MouseEvent a, Dia a) -> IO ()) (ModifiedKey -> IO ()) (IO (Dia a)) (MVar (Dia a))


runWidget :: Widget (RefCreator) -> IO ()
runWidget desc = do
    hSetBuffering stdout NoBuffering

    ch <- newChan
    (widget, setTime) <- runRefCreatorPost (writeChan ch) $ \post -> do
        i <- inCanvas 800 600 30 desc
        case i of
            Canvas w h sc_ me keyh r diaFun -> do
                rer <- liftIO' $ newMVar mempty
                rer' <- liftIO' $ newMVar mempty
                _ <- onChangeEq r $ \b -> liftIO' $ do
                    let d = diaFun b
                    _ <- tryTakeMVar rer
                    putMVar rer d
                    _ <- swapMVar rer' d
                    pure ()

                let keyhandle key = post $ fromMaybe (\_ -> pure False) keyh key >> pure ()

                return $ SWidget w h sc_ (post . me) keyhandle (readMVar rer') rer

    _ <- forkIO $ forever $ do
        m <- readChan ch
        setTime
        m

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
                let dia = dia_ # clearValue # scale sc # clipped (rect w h) <>
                            rect w h # fc white # lwL 0

                image <- createImage win sw sh dia
                copyImage (image, Rect 0 0 sw sh) (Screen win, Rect 0 sh sw 0)
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
trKey (GLFW.ModifierKeys s c a sup) k = case k of
    GLFW.Key'Space      -> char ' '
    GLFW.Key'Apostrophe -> ch '\'' '\"'
    GLFW.Key'Comma      -> ch ',' '<'
    GLFW.Key'Minus      -> ch '-' '_'
    GLFW.Key'Period     -> ch '.' '>'
    GLFW.Key'Slash      -> ch '/' '?'
    GLFW.Key'0 -> ch '0' ')'
    GLFW.Key'1 -> ch '1' '!'
    GLFW.Key'2 -> ch '2' '@'
    GLFW.Key'3 -> ch '3' '#'
    GLFW.Key'4 -> ch '4' '$'
    GLFW.Key'5 -> ch '5' '%'
    GLFW.Key'6 -> ch '6' '^'
    GLFW.Key'7 -> ch '7' '&'
    GLFW.Key'8 -> ch '8' '*'
    GLFW.Key'9 -> ch '9' '('
    GLFW.Key'Semicolon -> ch ';' ':'
    GLFW.Key'Equal -> ch '=' '+'
    GLFW.Key'A -> alpha 'a'
    GLFW.Key'B -> alpha 'b'
    GLFW.Key'C -> alpha 'c'
    GLFW.Key'D -> alpha 'd'
    GLFW.Key'E -> alpha 'e'
    GLFW.Key'F -> alpha 'f'
    GLFW.Key'G -> alpha 'g'
    GLFW.Key'H -> alpha 'h'
    GLFW.Key'I -> alpha 'i'
    GLFW.Key'J -> alpha 'j'
    GLFW.Key'K -> alpha 'k'
    GLFW.Key'L -> alpha 'l'
    GLFW.Key'M -> alpha 'm'
    GLFW.Key'N -> alpha 'n'
    GLFW.Key'O -> alpha 'o'
    GLFW.Key'P -> alpha 'p'
    GLFW.Key'Q -> alpha 'q'
    GLFW.Key'R -> alpha 'r'
    GLFW.Key'S -> alpha 's'
    GLFW.Key'T -> alpha 't'
    GLFW.Key'U -> alpha 'u'
    GLFW.Key'V -> alpha 'v'
    GLFW.Key'W -> alpha 'w'
    GLFW.Key'X -> alpha 'x'
    GLFW.Key'Y -> alpha 'y'
    GLFW.Key'Z -> alpha 'z'
    GLFW.Key'LeftBracket    -> ch '[' '{'
    GLFW.Key'Backslash      -> ch '\\' '|'
    GLFW.Key'RightBracket   -> ch ']' '}'
--    GLFW.Key'GraveAccent -> Key'
--    GLFW.Key'World1 -> Key'
--    GLFW.Key'World2 -> Key'
    GLFW.Key'Escape     -> f Key'Escape
    GLFW.Key'Enter      -> char '\n'
    GLFW.Key'Tab        -> char '\t'
    GLFW.Key'Backspace  -> f Key'Backspace
    GLFW.Key'Insert     -> f Key'Insert
    GLFW.Key'Delete     -> f Key'Delete
    GLFW.Key'Right      -> f Key'Right
    GLFW.Key'Left       -> f Key'Left
    GLFW.Key'Down       -> f Key'Down
    GLFW.Key'Up         -> f Key'Up
    GLFW.Key'PageUp     -> f Key'PageUp
    GLFW.Key'PageDown   -> f Key'PageDown
    GLFW.Key'Home       -> f Key'Home
    GLFW.Key'End        -> f Key'End
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
    _ -> f Key'Unknown
  where
    alpha = ModifiedKey False c a sup . Key'Char . if s then toUpper else id

    char = f . Key'Char

    f = ModifiedKey s c a sup

    ch x y = ModifiedKey False c a sup . Key'Char $ if s then y else x



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

#ifdef __RASTERIFIC__
    let sizeSpec = mkSizeSpec (Just $ fromIntegral width) (Just $ fromIntegral height)
    let JP.Image _ _ pixelData = renderDia Rasterific (RasterificOptions sizeSpec) dia
    unsafeWith pixelData $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (fromIntegral width) (fromIntegral height) 0 (fromIntegral gl_RGBA) gl_UNSIGNED_BYTE
#else
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
#endif

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
