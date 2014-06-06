{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | Lens-based Gtk interface
module LGtk.Key where

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

pattern CharKey c    = SimpleKey (Key'Char c) -- <- (getSimpleChar -> Just c)
{-
getSimpleChar (SimpleKey (Key'Char c)) = Just c
getSimpleChar (ShiftKey (Key'Char c)) = Just c
getSimpleChar _ = Nothing
-}
