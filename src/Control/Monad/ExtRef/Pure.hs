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

runLens_ :: Reader a (Lens_ a b) -> Lens' a b
runLens_ r f a = cloneLens (unLens_ $ runReader r a) f a

type LSt = Seq CC

data CC = forall a . CC (LSt -> a -> a) a

initLSt :: LSt
initLSt = empty

instance Reference (Lens_ LSt) where
    type RefState (Lens_ LSt) = State LSt

    readRef = view . runLens_
    writeRef r a = runLens_ r .= a
    lensMap l r = return $ Lens_ $ runLens_ r . l
    unitRef = return $ Lens_ united

instance ExtRef (State LSt) where
    type RefCore (State LSt) = EqRefCore (Lens_ LSt)

    liftWriteRef = id

    extRef r1_ r2 a0 = do
        EqRefCore r1 _k <- liftReadRef r1_
        q <- extRef' (cloneLens $ unLens_ r1) r2 a0
        return $ return $ EqRefCore (Lens_ q) $ const True
     where
      extRef' :: Lens' LSt b -> Lens' a b -> a -> State LSt (ALens' LSt a)
      extRef' r1 r2 a0 = state extend
       where
        rk = set r1 . (^. r2)
        kr = set r2 . (^. r1)

        extend x0 = (lens get set, x0 |> CC kr (kr x0 a0))
          where
            limit = (id *** toList) . splitAt (length x0)

            get = unsafeData . head . snd . limit

            set x a = foldl (\x -> (|>) x . ap_ x) (rk a zs |> CC kr a) ys where
                (zs, _ : ys) = limit x

        ap_ :: LSt -> CC -> CC
        ap_ x (CC f a) = CC f (f x a)

        unsafeData :: CC -> a
        unsafeData (CC _ a) = unsafeCoerce a
