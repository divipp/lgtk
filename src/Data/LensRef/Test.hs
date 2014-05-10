{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Tests for the @ExtRef@ interface.
module Data.LensRef.Test
    ( -- * Tests for the interface
      mkTests
    -- * Tests for implementations
    , testExtPure
    , testExtFast
    ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Concurrent
import Control.Category
import Control.Arrow ((***))
import Data.Maybe
import Prelude hiding ((.), id)
import System.IO

import Control.Lens
import Data.LensRef
import qualified Data.LensRef.Pure as Pure
import qualified Data.LensRef.Fast as Fast

import System.IO.Unsafe

-----------------------------------------------------------------


instance (ExtRef m, Monoid w) => RefReader_ (WriterT w m) where

    type RefCore (WriterT w m) = RefCore m

    liftReadRef = lift . liftReadRef


-- | This instance is used in the implementation, end users do not need it.
instance (ExtRef m, Monoid w) => ExtRef (WriterT w m) where

    extRef x y a = lift $ extRef x y a

instance (ExtRefWrite m, Monoid w) => ExtRefWrite (WriterT w m) where

    liftWriteRef = lift . liftWriteRef

{-
-- | Consistency tests for the pure implementation of @Ext@, should give an empty list of errors.
testExtPure :: [String]
testExtPure = mkTests $ \t -> let
    (((), m), w) = runWriter $ Pure.runPure newChan t
    in w -- ++ execWriter m
  where
    newChan = return (fail "x", \_ -> return (error "1"))
-}

instance MonadWriter [String] IO where
    tell = putStrLn . show

testExtPure = mkTests $ \t -> unsafePerformIO $ do
    hSetBuffering stdout LineBuffering
    ((), m) <- Pure.runPure newChan' t

    _ <- forkIO m
    return ["end"]
   where
    newChan' :: IO (IO a, a -> IO ())
    newChan' = do
        ch <- newChan
        return (readChan ch, \x -> writeChan ch x)

testExtFast = mkTests $ \t -> unsafePerformIO $ do
    hSetBuffering stdout LineBuffering
    ((), m) <- Fast.runPure newChan' t

    _ <- forkIO m
    return ["end"]
   where
    newChan' :: IO (IO a, a -> IO ())
    newChan' = do
        ch <- newChan
        return (readChan ch, \x -> writeChan ch x)


-- | Check an equality.
(==?) :: (Eq a, Show a, MonadWriter [String] (EffectM m), EffRef m) => a -> a -> m ()
rv ==? v = liftEffectM $ when (rv /= v) $ tell . return $ "runTest failed: " ++ show rv ++ " /= " ++ show v

-- | Check the current value of a given reference.
(==>) :: (Eq a, Show a, MonadWriter [String] (EffectM m), EffRef m) => Ref m a -> a -> m ()
r ==> v = readRef r >>= (==? v)

infix 0 ==>, ==?

maybeLens :: Lens' (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\(_,a) x -> maybe (False, a) (\a' -> (True, a')) x)

writeRef' = writeRef

{- | 
@mkTests@ generates a list of error messages which should be emtpy.

Look inside the sources for the tests.
-}
mkTests :: ((forall m . (MonadWriter [String] (EffectM m), EffRef m, ExtRefWrite m) => m ()) -> [String]) -> [String]
mkTests runTest
      = newRefTest
     ++ writeRefsTest
     ++ extRefTest
     ++ joinTest
     ++ joinTest2
     ++ chainTest0
     ++ forkTest
     ++ forkTest2
     ++ chainTest
     ++ chainTest'
     ++ undoTest
     ++ undoTest2
     ++ undoTest3

     ++ writeRefTest
  where

    newRefTest = runTest $ do
        r <- newRef (3 :: Int)
        r ==> 3


    writeRefsTest = runTest $ do
        r1 <- newRef (3 :: Int)
        r2 <- newRef (13 :: Int)
        r1 ==> 3
        r2 ==> 13
        writeRef r1 4
        r1 ==> 4
        r2 ==> 13
        writeRef r2 0
        r1 ==> 4
        r2 ==> 0

    extRefTest = runTest $ do
        r <- newRef $ Just (3 :: Int)
        q <- extRef r maybeLens (False, 0)
        let q1 = _1 `lensMap` q
            q2 = _2 `lensMap` q
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
        r2 <- newRef (5 :: Int)
        r1 <- newRef 3
        rr <- newRef r1
        r1 ==> 3
        let r = join' rr
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
        r1 <- newRef (3 :: Int)
        rr <- newRef r1
        r2 <- newRef 5
        writeRef rr r2
        join' rr ==> 5

    chainTest0 = runTest $ do
        r <- newRef (1 :: Int)
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
        r <- newRef (1 :: Int)
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
        r <- newRef $ Just (1 :: Int)
        q <- extRef r maybeLens (False, 0)
        s <- extRef r maybeLens (False, 0)
        r ==> Just 1
        q ==> (True, 1)
        s ==> (True, 1)
        writeRef r $ Just 2
        r ==> Just 2
        q ==> (True, 2)
        s ==> (True, 2)
        writeRef r Nothing
        r ==> Nothing
        q ==> (False, 2)
        s ==> (False, 2)
        writeRef (_1 `lensMap` q) True
        r ==> Just 2
        q ==> (True, 2)
        s ==> (True, 2)
        writeRef (_2 `lensMap` q) 3
        r ==> Just 3
        q ==> (True, 3)
        s ==> (True, 3)
        writeRef (_1 `lensMap` q) False
        r ==> Nothing
        q ==> (False, 3)
        s ==> (False, 3)
        writeRef (_2 `lensMap` q) 4
        r ==> Nothing
        q ==> (False, 4)
        s ==> (False, 3)
        writeRef (_1 `lensMap` q) True
        r ==> Just 4
        q ==> (True, 4)
        s ==> (True, 4)
        writeRef q (False, 5)
        r ==> Nothing
        q ==> (False, 5)
        s ==> (False, 4)
        writeRef (_1 `lensMap` s) True
        r ==> Just 4
        q ==> (True, 4)
        s ==> (True, 4)

    chainTest = runTest $ do
        r <- newRef $ Just Nothing
        q <- extRef r maybeLens (False, Nothing)
        s <- extRef (_2 `lensMap` q) maybeLens (False, 3 :: Int)
        writeRef (_1 `lensMap` s) False
        r ==> Just Nothing
        q ==> (True, Nothing)
        s ==> (False, 3)
        writeRef (_1 `lensMap` q) False
        r ==> Nothing
        q ==> (False, Nothing)
        s ==> (False, 3)

    chainTest' = runTest $ do
        r <- newRef $ Just $ Just (3 :: Int)
        q <- extRef r maybeLens (False, Nothing)
        s <- extRef (_2 `lensMap` q) maybeLens (False, 0 :: Int)
        r ==> Just (Just 3)
        q ==> (True, Just 3)
        s ==> (True, 3)
        writeRef (_1 `lensMap` s) False
        r ==> Just Nothing
        q ==> (True, Nothing)
        s ==> (False, 3)
        writeRef (_1 `lensMap` q) False
        r ==> Nothing
        q ==> (False, Nothing)
        s ==> (False, 3)
        writeRef (_1 `lensMap` s) True
        r ==> Nothing
        q ==> (False, Just 3)
        s ==> (True, 3)
        writeRef (_1 `lensMap` q) True
        r ==> Just (Just 3)
        q ==> (True, Just 3)
        s ==> (True, 3)

    undoTest = runTest $ do
        r <- newRef (3 :: Int)
        q <- extRef r (lens head $ flip (:)) []
        writeRef r 4
        q ==> [4, 3]

    undoTest2 = runTest $ do
        r <- newRef (3 :: Int)
        q <- extRef r (lens head $ flip (:)) []
        q ==> [3]

    undoTest3 = runTest $ do
        r <- newRef (3 :: Int)
        (undo, redo) <- liftM (liftReadRef *** liftReadRef) $ undoTr (==) r
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
        push m = m >>= \x -> maybe (return ()) liftWriteRef x
        m === t = m >>= \x -> isJust x ==? t



    writeRefTest = runTest $ do
        r <- newRef (3 :: Int)
        k <- newRef (3 :: Int)
        sr <- toReceive (writeRef' r) (const $ return ())
        sk <- toReceive (writeRef' k) (const $ return ())

        _ <- onChange (readRef r) $ \x -> do
            when (x == 3) $ do
                _ <- onChange (readRef k) $ \x -> do
                    liftEffectM $ tell ["k1: " ++ show x]
                    return $ return ()
                return ()
            return $ do
                liftEffectM $ tell ["r: " ++ show x]
                when (x == 4) $ do
                    _ <- onChange (readRef k) $ \x -> return $ do
                        liftEffectM $ tell ["k2: " ++ show x]
                    return ()
                r ==> x
        r ==> 3
        liftEffectM $ sr 5
        liftEffectM $ sk 6
        liftEffectM $ sr 4
        liftEffectM $ sk 7
        liftEffectM $ sr 3
        liftEffectM $ sk 8
        liftEffectM $ sr 5
        liftEffectM $ sk 9
        liftEffectM $ sr 4
        liftEffectM $ sk 10
        liftEffectM $ sr 3
        liftEffectM $ sk 11
        r ==> 3
        return ()

    join' r = join $ readRef r

-- | Undo-redo state transformation.
undoTr
    :: EffRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a             -- ^ reference of state
    ->   m ( ReadRef m (Maybe (WriteRef m ()))
           , ReadRef m (Maybe (WriteRef m ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extRef r (undoLens eq) ([], [])
    let try f = liftM (liftM (writeRef_ ku) . f) $ readRef ku
    return (try undo, try redo)
  where
    undo (x: xs@(_:_), ys) = Just (xs, x: ys)
    undo _ = Nothing

    redo (xs, y: ys) = Just (y: xs, ys)
    redo _ = Nothing

undoLens :: (a -> a -> Bool) -> Lens' ([a],[a]) a
undoLens eq = lens get set where
    get = head . fst
    set (x' : xs, ys) x | eq x x' = (x: xs, ys)
    set (xs, _) x = (x : xs, [])


