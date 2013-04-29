-- | This module exports unsafe definitions
module Control.MLens.Unsafe
    ( -- * Unsafe references
      fileRef

    -- * Unsafe lifting
    , unsafeC
    , unsafeR
    , unsafeCToR
    ) where

import Control.Monad.Register
import Control.Monad.Restricted


