{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tests for the reference implementation of the @ExtRef@ interface.
module Control.MLens.ExtRef.Test
    ( -- * Basic test environment
      (==?)
    , (==>)
    -- * Test suit generation
    , mkTests
    , mkTests'
    ) where

import Control.Monad.Writer
import Control.Category
import Prelude hiding ((.), id)

import Data.MLens
import Data.MLens.Ref
import Control.MLens.ExtRef

-----------------------------------------------------------------

-- | Check an equality.
(==?) :: (Eq a, Show a, MonadWriter [String] m) => a -> a -> m ()
rv ==? v = when (rv /= v) $ tell . return $ "runTest failed: " ++ show rv ++ " /= " ++ show v

-- | Check the current value of a given reference.
(==>) :: (Eq a, Show a, MonadWriter [String] m) => Ref m a -> a -> m ()
r ==> v = readRef r >>= (==? v)

infix 0 ==>, ==?

newtype F m i a = F { unF :: m a } deriving (Monad, NewRef, ExtRef, MonadWriter w)

-- | Simplified test generation
mkTests' :: (MonadWriter [String] m, ExtRef m) => (m () -> [String]) -> [String]
mkTests' f = mkTests $ \m -> f $ unF m

{- | 
@mkTests@ generates a list of error messages which should be emtpy.

Look inside the sources for the tests.
-}
mkTests :: ((forall i . (MonadWriter [String] (m i), ExtRef (m i)) => m i ()) -> [String]) -> [String]
mkTests runTest
      = newRefTest
     ++ writeRefTest
     ++ writeRefsTest
     ++ extRefTest
     ++ joinTest
     ++ joinTest2
     ++ undoTest
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

    undoTest = runTest $ do
        r <- newRef 3
        q <- extRef r (lens head (:)) []
        writeRef r 4
        q ==> [4, 3]




