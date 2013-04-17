{-# LANGUAGE RankNTypes #-}
module Data.MLens.Ref
    ( -- * Data type for reference lenses
      Ref

    -- * Reference operations
    , readRef, writeRef, modRef

    -- * Some @IO@ referenceses
    , fileRef, fileRef_
    , logConsoleLens

    -- * Auxiliary definitions
    , logMLens, logFile
    ) where

import Control.Monad
import Control.Category
import System.Directory
import Prelude hiding ((.), id)

import Data.MLens

{- |
Note that references lenses can be composed with lenses.
For example, if

@r :: Ref m (a,b)@

then

@fstLens . r :: Ref m a@

Reference laws for pure references:

 *  @(readRef r)@ has no side effect.
 *  @(readRef r >>= writeRef r)@ has no side effect.
 *  @(writeRef r a >> readRef r)@ returns @a@.
 *  @(writeRef r a >> writeRef r a)@ has the same effect as @(writeRef r a)@.
-}
type Ref m a = MLens m () a

readRef :: Monad m => MLens m () a -> m a
readRef k = getL k ()

writeRef :: Monad m => Ref m a -> a -> m ()
writeRef r a = setL r a ()

modRef :: Monad m => Ref m a -> (a -> a) -> m ()
k `modRef` f = modL k f ()

-- | Using @fileRef@ is safe if the file is not used concurrently.
fileRef :: FilePath -> IO (Ref IO String)
fileRef f = liftM (justLens "" .) $ fileRef_ f

-- | Note that if you write @Nothing@, the file is deleted.
fileRef_ :: FilePath -> IO (Ref IO (Maybe String))
fileRef_ f = return $ MLens $ \() -> do
    b <- doesFileExist f
    if b then do
            xs <- readFile f
            length xs `seq` return (Just xs, wr)
         else return (Nothing, wr)
 where wr = maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f)

logMLens :: Monad m => (a -> m ()) -> (a -> m ()) -> MLens m a a
logMLens getLog setLog = MLens $ \a -> getLog a >> return (a, \b -> setLog b >> return b)

{- |
@logConsoleLens@ logs elementary get and set operations.

Note that with the current representation of @MLens@, every set operation involves a get operation.
-}
logConsoleLens :: (Show a) => MLens IO a a
logConsoleLens = logMLens (putStrLn . ("Get: " ++) . show) (putStrLn . ("Set: " ++) . show)

logFile :: FilePath -> IO (String -> IO ())
logFile f = do
    b <- doesFileExist f
    when (not b) $ writeFile f ""
    return $ appendFile f



