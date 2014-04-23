{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Pure reference implementation for the @ExtRef@ interface.

The implementation uses @unsafeCoerce@ internally, but its effect cannot escape.
-}
module Control.Monad.ExtRef.Pure where

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***))
import Data.Sequence hiding (singleton)
import Control.Lens hiding ((|>))
import Data.Foldable (toList)
import Prelude hiding (splitAt, length)

import Unsafe.Coerce

import Control.Monad.ExtRef

----------------------

newtype Lens_ a b = Lens_ {unLens_ :: ALens' a b}

runLens_ :: Lens_ a b -> Lens' a b
runLens_ r = cloneLens $ unLens_ r

type LSt = Seq CC

data CC = forall a . CC (LSt -> a -> a) a

initLSt :: LSt
initLSt = empty

instance Reference (Lens_ LSt) where
    type RefState (Lens_ LSt) = State LSt

    readRef m = do
        r <- m
        gets $ runReader $ view $ runLens_ r
    writeRef m a = do
        r <- m
        runLens_ r .= a
    lensMap l m = do
        r <- m
        return $ Lens_ $ runLens_ r . l
    unitRef = return $ Lens_ united

instance ExtRef (State LSt) where
    type RefCore (State LSt) = Lens_ LSt

    liftWriteRef = id

    extRef mr r2 a0 = do
     r <- mr
     er r where

     er r =state extend
      where
        rk = set (runLens_ r) . (^. r2)
        kr = set r2 . (^. runLens_ r)

        extend x0 = (return $ Lens_ $ lens get set, x0 |> CC kr (kr x0 a0))
          where
            limit = (id *** toList) . splitAt (length x0)

            get = unsafeData . head . snd . limit

            set x a = foldl (\x -> (|>) x . ap_ x) (rk a zs |> CC kr a) ys where
                (zs, _ : ys) = limit x

        ap_ :: LSt -> CC -> CC
        ap_ x (CC f a) = CC f (f x a)

        unsafeData :: CC -> a
        unsafeData (CC _ a) = unsafeCoerce a

--    type PureExt (State LSt) = State LSt

    lazyExtRef m f = do
        s <- newRef (Nothing, [])
        return $ do
            x <- m
            (la, ms) <- readRef s
            case (la, lookup x ms) of
                (Just (x', y), _) | x' == x -> return y
                (_, Just m) -> do
                    y <- m
                    writeRef s (Just (x, y), ms)
                    return y
                _ -> do
                    m <- f x
                    y <- m
                    writeRef s (Just (x, y), (x, m): ms)
                    return y


