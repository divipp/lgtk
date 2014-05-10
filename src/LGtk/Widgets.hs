{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Lens-based Gtk interface
module LGtk.Widgets
    ( module LGtk.Widgets
    , Colour, RGB (..), sRGB, toSRGB
    ) where

import Data.Semigroup
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Prelude (QDiagram, R2, P2)
import Diagrams.Backend.Cairo (B)

---------------------------------------------------------

import Data.LensRef

type Receive m a = a -> Modifier m ()

type SendReceive m a = (ReadRef m a, Receive m a)

type Widget m = m (WidgetCore m)

-- | Widget descriptions
data WidgetCore m
    = Label (ReadRef m String)     -- ^ label
    | Button { label_  :: ReadRef m String
             , sensitive_ :: ReadRef m Bool
             , color_ :: Maybe (ReadRef m (Colour Double))
             , action_ :: Receive m ()
             }  -- ^ button
    | Checkbox (SendReceive m Bool)         -- ^ checkbox
    | Combobox [String] (SendReceive m Int) -- ^ combo box
    | Entry (String -> Bool) (SendReceive m String)          -- ^ entry field
    | List ListLayout [Widget m]         -- ^ group interfaces into row or column
    | Notebook' (Receive m Int) [(String, Widget m)]     -- ^ actual tab index, tabs
    | forall b . Eq b => Cell (ReadRef m b) (forall x . (Widget m -> m x) -> b -> m (m x))
    | forall a b . (Eq b, Monoid a, Semigroup a)
    => Canvas
        Int     -- width
        Int     -- height
        Double  -- scale
        ((MouseEvent a, Dia a) -> Modifier m ())    -- mouse event handler
        (KeyboardHandler (Modifier m))              -- keyboard event handler
        (ReadRef m b)
        (b -> Dia a)
    | Scale Double Double Double (SendReceive m Double)

data Key
    = Key'Char Char
    | Key'Escape
    | Key'Backspace
    | Key'Insert
    | Key'Delete
    | Key'Right
    | Key'Left
    | Key'Down
    | Key'Up
    | Key'PageUp
    | Key'PageDown
    | Key'Home
    | Key'End
    | Key'Unknown
        deriving (Eq, Ord, Read, Show)

pattern Key'Tab   = Key'Char '\t'
pattern Key'Enter = Key'Char '\n'
pattern Key'Space = Key'Char ' '

data ModifiedKey = ModifiedKey
    { modifierKeysShift   :: Bool
    , modifierKeysControl :: Bool
    , modifierKeysAlt     :: Bool
    , modifierKeysSuper   :: Bool
    , theModifiedKey      :: Key
    }
        deriving (Eq, Ord, Read, Show)

pattern SimpleKey k  = ModifiedKey False False False False k
pattern ShiftKey k   = ModifiedKey True  False False False k
pattern ControlKey k = ModifiedKey False True  False False k
pattern AltKey k     = ModifiedKey False False True  False k
pattern SuperKey k   = ModifiedKey False False False True  k

pattern CharKey c   = SimpleKey (Key'Char c)


type Dia a = QDiagram B R2 a

data ListLayout
    = Horizontal
    | Vertical
        deriving (Eq)

type ScrollDirection = ListLayout

type KeyboardHandler m = Maybe (ModifiedKey -> m Bool)

data MouseEvent a
    = MoveTo (MousePos a)
    | MouseEnter (MousePos a)
    | MouseLeave (MousePos a)
    | Click (MousePos a)
    | DragTo (MousePos a)
    | Release (MousePos a)
    | ScrollTo ScrollDirection (MousePos a)
    | LostFocus
    | GetFocus
        deriving (Eq)

data MousePos a
    = MousePos P2 a
        deriving (Eq)
