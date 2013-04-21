-- | This module exports unsafe definitions, like the @NewRef@ instance for @IO@ which does not fulfil the @NewRef@ laws in a multi-threaded environment.
module Control.MLens.Unsafe
    ( -- * Unsafe references
      fileRef
--    , fileRef_
    ) where

import Control.Monad.Restricted
import qualified Data.MLens.Ref as M
import Control.MLens.NewRef.Unsafe ()

fileRef :: FilePath -> C IO (M.Ref IO String)
fileRef = C . M.fileRef

