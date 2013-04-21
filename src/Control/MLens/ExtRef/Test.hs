{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Tests for the reference implementation of the @ExtRef@ interface.
module Control.MLens.ExtRef.Test
    ( -- * Test suit generation
      mkTests
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
     ++ chainTest0
     ++ forkTest
     ++ forkTest2
     ++ chainTest
     ++ undoTest
     ++ undoTest2
     ++ undoTest3
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

    chainTest0 = runTest $ do
        r <- newRef 1
        q <- extRef r id 0
        s <- extRef q id 0
        r ==> 1
        q ==> 1
        s ==> 1
        writeRef r 2
        r ==> 2
        q ==> 2
        s ==> 2
        writeRef q 3
        r ==> 3
        q ==> 3
        s ==> 3
        writeRef s 4
        r ==> 4
        q ==> 4
        s ==> 4

    forkTest = runTest $ do
        r <- newRef 1
        q <- extRef r id 0
        s <- extRef r id 0
        r ==> 1
        q ==> 1
        s ==> 1
        writeRef r 2
        r ==> 2
        q ==> 2
        s ==> 2
        writeRef q 3
        r ==> 3
        q ==> 3
        s ==> 3
        writeRef s 4
        r ==> 4
        q ==> 4
        s ==> 4

    forkTest2 = runTest $ do
        r <- newRef $ Just 1
        q <- extRef r maybeLens (False, 0)
        s <- extRef r maybeLens (False, 0)
        r ==> Just 1
        q ==> (True, 1)
        s ==> (True, 1)
        writeRef r $ Just 2
        r ==> Just 2
        q ==> (True, 2)
        s ==> (True, 2)
        writeRef r $ Nothing
        r ==> Nothing
        q ==> (False, 2)
        s ==> (False, 2)
        writeRef (fstLens . q) True
        r ==> Just 2
        q ==> (True, 2)
        s ==> (True, 2)
        writeRef (sndLens . q) 3
        r ==> Just 3
        q ==> (True, 3)
        s ==> (True, 3)
        writeRef (fstLens . q) False
        r ==> Nothing
        q ==> (False, 3)
        s ==> (False, 3)
        writeRef (sndLens . q) 4
        r ==> Nothing
        q ==> (False, 4)
        s ==> (False, 3)
        writeRef (fstLens . q) True
        r ==> Just 4
        q ==> (True, 4)
        s ==> (True, 4)
        writeRef q (False, 5)
        r ==> Nothing
        q ==> (False, 5)
        s ==> (False, 4)
        writeRef (fstLens . s) True
        r ==> Just 4
        q ==> (True, 4)
        s ==> (True, 4)

    chainTest = runTest $ do
        r <- newRef $ Just $ Just 3
        q <- extRef r maybeLens (False, Nothing)
        s <- extRef (sndLens . q) maybeLens (False, 0)
        r ==> Just (Just 3)
        q ==> (True, Just 3)
        s ==> (True, 3)
        writeRef (fstLens . s) False
        r ==> Just Nothing
        q ==> (True, Nothing)
        s ==> (False, 3)
        writeRef (fstLens . q) False
        r ==> Nothing
        q ==> (False, Nothing)
        s ==> (False, 3)
        writeRef (fstLens . s) True
        r ==> Nothing
        q ==> (False, Just 3)
        s ==> (True, 3)
        writeRef (fstLens . q) True
        r ==> Just (Just 3)
        q ==> (True, Just 3)
        s ==> (True, 3)

    undoTest = runTest $ do
        r <- newRef 3
        q <- extRef r (lens head (:)) []
        writeRef r 4
        q ==> [4, 3]

    undoTest2 = runTest $ do
        r <- newRef 3
        q <- extRef r (lens head (:)) []
        q ==> [3]

    undoTest3 = runTest $ do
        r <- newRef 3
        (undo, redo) <- undoTr (==) r
        r ==> 3
        redo === False
        undo === False
        writeRef r 4
        r ==> 4
        redo === False
        undo === True
        writeRef r 5
        r ==> 5
        redo === False
        undo === True
        push undo
        r ==> 4
        redo === True
        undo === True
        push undo
        r ==> 3
        redo === True
        undo === False
        push redo
        r ==> 4
        redo === True
        undo === True
        writeRef r 6
        r ==> 6
        redo === False
        undo === True
      where
        push m = m >>= \x -> maybe (return ()) id x
        m === t = m >>= \x -> maybe False (const True) x ==? t

