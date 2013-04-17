{-# LANGUAGE RankNTypes #-}
-- | Tests for the reference implementation of the @ExtRef@ interface.
module Control.MLens.ExtRef.Pure.Test
    ( -- * Basic test environment
      Test, (==>), runTest
    -- * Test suit 
    , tests
    ) where

import Control.Monad.Writer
import Control.Category
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.ExtRef.Pure

-----------------------------------------------------------------

-- | Tests use a writer monad to tell errors.
type Test i = Ext i (Writer [String])

-- | This operator checks the current value of a given reference.
(==>) :: (Eq a, Show a) => Ref (Test i) a -> a -> Test i ()
r ==> v = readRef r >>= \rv -> when (rv /= v) $ lift . tell . return $ "runTest failed: " ++ show rv ++ " /= " ++ show v

infix 0 ==>

-- | Test running results the list of error messages given by @(==>)@.
runTest :: (forall i . Test i a) -> [String]
runTest = execWriter . runExt 

--------------------

{- | 
@tests@ contains error messages; it should be emtpy.

Look inside the sources for the tests.
-}
tests :: [String] 
tests = newRefTest
     ++ writeRefTest
     ++ writeRefsTest
     ++ extRefTest
     ++ joinTest
     ++ joinTest2
  where

    newRefTest = runTest $ do
        r <- newRef 3
        r ==> 3

    writeRefTest = runTest $ do
        r <- newRef 3
        r ==> 3
        writeRef r 4
        r ==> 4

    writeRefsTest = runTest $ do
        r1 <- newRef 3
        r2 <- newRef 13
        r1 ==> 3
        r2 ==> 13
        writeRef r1 4
        r1 ==> 4
        r2 ==> 13
        writeRef r2 0
        r1 ==> 4
        r2 ==> 0

    extRefTest = runTest $ do
        r <- newRef $ Just 3
        q <- extRef r maybeLens (False, 0)
        let q1 = fstLens . q
            q2 = sndLens . q
        r ==> Just 3
        q ==> (True, 3)
        writeRef r Nothing
        r ==> Nothing
        q ==> (False, 3)
        q1 ==> False
        writeRef q1 True
        r ==> Just 3
        writeRef q2 1
        r ==> Just 1

    joinTest = runTest $ do
        r2 <- newRef 5
        r1 <- newRef 3
        rr <- newRef r1
        r1 ==> 3
        let r = joinLens rr
        r ==> 3
        writeRef r1 4
        r ==> 4
        writeRef rr r2
        r ==> 5
        writeRef r1 4
        r ==> 5
        writeRef r2 14
        r ==> 14

    joinTest2 = runTest $ do
        r1 <- newRef 3
        rr <- newRef r1
        r2 <- newRef 5
        writeRef rr r2
        joinLens rr ==> 5



