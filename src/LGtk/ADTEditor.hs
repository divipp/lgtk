{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module LGtk.ADTEditor
    ( List (..), Elems(..), ADTLens(..)
    , adtEditor
    ) where

import LGtk

import Control.Monad
import Prelude hiding ((.), id)

-- | Type-level lists
data List a = Nil | Cons a (List a)

-- | Heterogeneous lists
data Elems (xs :: List *) where
    ElemsNil :: Elems Nil
    ElemsCons :: ADTLens a => a -> Elems as -> Elems (Cons a as)

-- | Lens for editable ADTs
class ADTLens a where
    type ADTEls a :: List *
    adtLens :: ([(String, [Int])], Elems (ADTEls a), Lens (Int, Elems (ADTEls a)) a)

-- | A generic ADT editor
adtEditor :: (EffRef m, ADTLens a) => Ref m a -> C m (Widget m)
adtEditor = liftM (Action . runC) . memoRead . editor  where
    editor r = do
        q <- extRef r k (0, ls)
        es <- mkEditors ls $ sndLens % q
        return $ hcat
            [ combobox (map fst ss) $ fstLens % q
            , cell True (liftM fst $ readRef q) $ \i -> return $ vcat [es !! j | j <- snd $ ss !! i]
            ]
      where
        (ss, ls, k) = adtLens

    mkEditors :: EffRef m => Elems xs -> Ref m (Elems xs) -> C m [Widget m]
    mkEditors ElemsNil _ = return []
    mkEditors (ElemsCons _ xs) r = do
        i <- adtEditor $ lHead % r
        is <- mkEditors xs $ lTail % r
        return $ i : is
      where
        lHead = lens get set where
            get :: Elems (Cons x xs) -> x
            get (ElemsCons a _) = a
            set :: x -> Elems (Cons x xs) -> Elems (Cons x xs)
            set a (ElemsCons _ as) = ElemsCons a as

        lTail = lens get set where
            get :: Elems (Cons x xs) -> Elems xs
            get (ElemsCons _ as) = as
            set :: Elems xs -> Elems (Cons x xs) -> Elems (Cons x xs)
            set as (ElemsCons a _) = ElemsCons a as


