-- | This module exports unsafe definitions, like the @NewRef@ instance for @IO@ which does not fulfil the @NewRef@ laws in a multi-threaded environment.
module Control.MLens.Unsafe
    ( -- * Unsafe references
      fileRef

    -- * Unsafe lifting
    , unsafeC
    , unsafeR
    , unsafeCToR
    ) where

import Control.Monad.Register
import Control.MLens.NewRef.Unsafe ()
import Control.Monad.Restricted


