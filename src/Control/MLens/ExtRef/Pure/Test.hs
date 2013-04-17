{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tests for the reference implementation of the @ExtRef@ interface.
module Control.MLens.ExtRef.Pure.Test
    ( -- * Test suit 
      tests
    ) where

import Control.Monad.Writer

import Control.MLens.ExtRef
import Control.MLens.ExtRef.Test
import Control.MLens.ExtRef.Pure


newtype E i a = E { unE :: Ext i (Writer [String]) a } deriving (Monad, MonadWriter [String], NewRef, ExtRef)

tests :: [String]
tests = mkTests (\t -> execWriter $ runExt $ unE t)

