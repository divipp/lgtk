{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
-- | A generic ADT editor defined on top of the main LGtk interface, "LGtk".
module LGtk.ADTEditor
    ( List (..), Elems(..), ADTLens(..)
    , adtEditor
    ) where

import LGtk

import Prelude hiding ((.), id)

-- | Type-level lists
data List a = Nil | Cons a (List a)

-- | Heterogeneous lists
data Elems (xs :: List *) where
    ElemsNil :: Elems Nil
    ElemsCons :: ADTLens a => a -> Elems as -> Elems (Cons a as)

{- | Lens for editable ADTs with support of shared record fields between constructors.

Suppose we have the data type

@
data X
    = X1 { a :: Int, b :: Bool }
    | X2 { a :: Int, c :: Char }
@

We can build an editor which can switch between two editor for the constructors.
If the field @a@ is edited in one editor, it will be updated in the other.
-}
class ADTLens a where

    {- | @ADTEls a@ is the list of types of the parts of the ADT.

    For example,

    @ADTEls X = Cons Int (Cons Bool (Cons Char Nil))@
    -}
    type ADTEls a :: List *

    {- | The lens which defines an abstract editor.

    The first parameter defines the displayed constructor name and the parts of the constructor for each constructor.
    @Int@ is an index in the @ADTEls@ list.

    For example, in case of @X@,

    @fst3 adtLens = [(\"X1\", [0, 1]), (\"X2\", [0, 2])]@

    The second parameter is the list of default values for each part.

    The third parameter is a lens from the selected constructor index plus
    the values of the ADT parts to the ADT values.
    -}
    adtLens :: ([(String, [Int])], Elems (ADTEls a), Lens (Int, Elems (ADTEls a)) a)

-- | A generic ADT editor
adtEditor :: (EffRef m, ADTLens a) => Ref m a -> m (Widget m)
adtEditor = liftM action . memoRead . editor  where
    editor r = do
        q <- extRef r k (0, ls)
        es <- mkEditors ls $ sndLens `lensMap` q
        return $ hcat
            [ combobox (map fst ss) $ fstLens `lensMap` q
            , cell (liftM fst $ readRef q) $ \i -> vcat [es !! j | j <- snd $ ss !! i]
            ]
      where
        (ss, ls, k) = adtLens

    mkEditors :: EffRef m => Elems xs -> Ref m (Elems xs) -> m [Widget m]
    mkEditors ElemsNil _ = return []
    mkEditors (ElemsCons _ xs) r = do
        i <- adtEditor $ lHead `lensMap` r
        is <- mkEditors xs $ lTail `lensMap` r
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


