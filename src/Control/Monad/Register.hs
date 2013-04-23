{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Register
    ( MonadRegister (..)
    , IC (..)
    , addFreeCEffect
    , addRefEffect
    , addPushEffect
    , EE
    , evalEE
    , evalEE_
    , unFree
    ) where

import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Restricted
import Data.MLens.Ref
import Control.MLens.ExtRef
import Control.MLens.ExtRef.Pure

data IC m a = forall b . Eq b => IC (R (Inn m) b) (b -> C m a)

class (Monad (Inn m), Monad m) => MonadRegister m where

    type Inn m :: * -> *

    liftInn :: Morph (Inn m) m

    addCEffect :: Eq a => C (Inn m) a -> (a -> Inn m ()) -> m ()

    addWEffect :: Eq a => (a -> Inn m ()) -> ((a -> m ()) -> Inn m x) -> m x

    addICEffect :: IC m a -> Inn m (a -> Inn m ()) -> m ()

    addMemoICEffect :: IC m a -> Inn m (a -> Inn m ()) -> m ()

addFreeCEffect :: (MonadRegister m, Functor (Inn m), Eq a) => Free (C (Inn m)) a -> (a -> Inn m ()) -> m ()
addFreeCEffect rb act = unFree (liftInn . act) (flip addCEffect act) rb

addRefEffect :: (MonadRegister m, Eq a) => Ref (Inn m) a -> ((a -> m ()) -> Inn m (a -> Inn m ())) -> m ()
addRefEffect r int = do
    act <- addWEffect (writeRef r) int
    addCEffect (rToC $ readRef r) act

addPushEffect :: MonadRegister m => Inn m () -> (m () -> Inn m ()) -> m ()
addPushEffect ma mb = addWEffect (const ma) $ \f -> mb $ f ()


instance (NewRef m) => NewRef (StateT s m) where
    type Inner (StateT s m) = Inner m

    liftInner = lift . liftInner

    newRef = mapC lift . newRef

instance (ExtRef m) => ExtRef (StateT s m) where
    extRef r k a = mapC lift $ extRef r k a


--------------------------------

newtype EE m a = EE { unEE :: StateT [m ()] m a }
    deriving (Functor, Monad, MonadState [m ()])

instance NewRef m => MonadRegister (EE m) where

    type Inn (EE m) = m

    liftInn = EE . lift

    addWEffect r int = do
        act <- liftInn $ int $ \a -> do
            liftInn $ r a
            acts <- get
            liftInn $ sequence_ acts
            return ()
        return act

    addCEffect rb act = do
        lastB <- liftInn $ runC $ newRef Nothing
        let reg = do
                b <- runC rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        liftInner $ writeRef lastB $ Just b
                        act b
        modify (++ [reg])
        return ()

    addICEffect (IC rb fb) int = do
        ir <- liftInn $ runC $ newRef []
        lastB <- liftInn $ runC $ newRef Nothing
        act <- liftInn int
        let reg = do
                b <- runR rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        (c, s) <- runStateT (unEE $ runC $ fb b) []
                        liftInner $ writeRef ir s
                        liftInner $ writeRef lastB $ Just b
                        act c
            getir = do
                s <- liftInner $ runR $ readRef ir
                sequence_ s
        modify (++ [reg, getir])
        return ()

    -- TODO: do not track events of inactive parts
    addMemoICEffect (IC rb fb) int = do
        ir <- liftInn $ runC $ newRef []
        lastB <- liftInn $ runC $ newRef Nothing
        prev <- liftInn $ runC $ newRef []
        act <- liftInn int
        let reg = do
                b <- runR rb
                mb <- liftInner $ runR $ readRef lastB
                case mb of
                    Just b' | b == b' -> return () 
                    _ -> do
                        prevs <- liftInner $ runR $ readRef prev
                        (c, s) <- case [x | (b', x) <- prevs, b' == b] of
                            [x] -> return x
                            [] -> do
                                x <- runStateT (unEE $ runC $ fb b) []
                                liftInner $ modRef prev ((b, x) :)
                                return x
                        liftInner $ writeRef ir s
                        liftInner $ writeRef lastB $ Just b
                        act c
            getir = do
                s <- liftInner $ runR $ readRef ir
                sequence_ s
        modify (++ [reg, getir])
        return ()


evalEE :: Monad m => EE m a -> m a
evalEE = flip evalStateT [] . unEE

evalEE_ :: forall m a . NewRef m => (Morph (EE m) m -> EE m a) -> m a
evalEE_ f = do
    vx <- runC $ newRef ([] :: [m ()])
    let unlift :: Morph (EE m) m
        unlift f = do
            x <- liftInner $ runR $ readRef vx
            (b, x) <- runStateT (unEE f) x
            liftInner $ writeRef vx x
            return b
    unlift $ f unlift

instance NewRef m => NewRef (EE m) where
    type Inner (EE m) = Inner m

    liftInner = EE . liftInner

    newRef = mapC EE . newRef

instance ExtRef m => ExtRef (EE m) where
    extRef r k a = mapC EE $ extRef r k a

unFree :: (Functor m, Monad m) => (a -> x) -> (m a -> x) -> Free m a -> x
unFree r m = evalFree r (m . join . fmap (induce id))

