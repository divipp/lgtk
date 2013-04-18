{-# LANGUAGE RankNTypes #-}
module Data.MLens.Ref
    ( Unit

    -- * Data type for reference lenses
    , Ref

    -- * Reference operations
    , runRef
    , readRef, writeRef, modRef

    -- * Some impure @IO@ referenceses
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
The abstract data type @Unit@ is isomorphic to @()@,
but by making it abstract we can prevent using the same reference
on both sides of lens composition, which would have surprising effects.
-}
data Unit = Unit deriving (Eq, Show)

{- |
Laws for pure references:

 *  @(readRef r >> return ())@ === @(return ())@

 *  @(readRef r >>= writeRef r)@ === @(return ())@

 *  @(writeRef r a >> readRef r)@ === @(return a)@

 *  @(writeRef r a >> writeRef r a')@ === @(writeRef r a')@

These laws are equivalent to the get-no-effect, set-get, get-set and set-set laws for monadic lenses.

Reference lenses can be composed with lenses.
For example, if

@r :: Ref m (a,b)@

then

@fstLens . r :: Ref m a@
-}
type Ref m a = MLens m Unit a

runRef :: Monad m => Ref m a -> m (a, a -> m ())
runRef r = liftM f $ runMLens r Unit where
    f (a, m) = (a, \a -> m a >> return ())

readRef :: Monad m => Ref m a -> m a
readRef = liftM fst . runRef

writeRef :: Monad m => Ref m a -> a -> m ()
writeRef r a = runRef r >>= ($ a) . snd

modRef :: Monad m => Ref m a -> (a -> a) -> m ()
k `modRef` f = runRef k >>= \(a, m) -> m $ f a

-- | Using @fileRef@ is safe if the file is not used concurrently.
fileRef :: FilePath -> IO (Ref IO String)
fileRef f = liftM (justLens "" .) $ fileRef_ f

-- | Note that if you write @Nothing@, the file is deleted.
fileRef_ :: FilePath -> IO (Ref IO (Maybe String))
fileRef_ f = return $ MLens $ \unit -> do
    b <- doesFileExist f
    if b then do
            xs <- readFile f
            length xs `seq` return (Just xs, wr unit)
         else return (Nothing, wr unit)
 where
    wr unit mb = do
        maybe (doesFileExist f >>= \b -> when b (removeFile f)) (writeFile f) mb
        return unit

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



