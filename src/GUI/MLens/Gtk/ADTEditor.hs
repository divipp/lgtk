{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module GUI.MLens.Gtk.ADTEditor
    ( List (..), Elems(..), ADTLens(..)
    , adtEditor
    ) where

import GUI.MLens.Gtk

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
adtEditor :: (ExtRef m, ADTLens a) => Ref m a -> m (I m)
adtEditor = liftM Action . memoRead . editor  where
    editor r = do
        q <- extRef r k (0, ls)
        es <- mkEditors ls $ sndLens % q
        return $ hcat
            [ Combobox (map fst ss) $ fstLens % q
            , Cell True (liftM fst $ runR $ readRef q) $ \i -> vcat [es !! j | j <- snd $ ss !! i]
            ]
      where
        (ss, ls, k) = adtLens

    mkEditors :: ExtRef m => Elems xs -> Ref m (Elems xs) -> m [I m]
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


