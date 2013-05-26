{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module LGtk.Demos.TEditor where

import Control.Monad
import Prelude hiding ((.), id)

import LGtk
import LGtk.ADTEditor

-- | Binary tree shapes
data T
    = Leaf
    | Node T T
        deriving Show

-- | Lens for @T@
tLens :: Lens (Bool, (T, T)) T
tLens = lens get set where
    get (False, _)     = Leaf
    get (True, (l, r)) = Node l r
    set Leaf (_, x)   = (False, x)
    set (Node l r) _ = (True, (l, r))

-- | @ADTLens@ instance for @T@
instance ADTLens T where
    type ADTEls T = Cons T (Cons T Nil)
    adtLens = ([("Leaf",[]),("Node",[0,1])], ElemsCons Leaf (ElemsCons Leaf ElemsNil), lens get set) where
        get :: (Int, Elems (ADTEls T)) -> T
        get (0, _)     = Leaf
        get (1, ElemsCons l (ElemsCons r ElemsNil)) = Node l r
        set :: T -> (Int, Elems (ADTEls T)) -> (Int, Elems (ADTEls T))
        set Leaf (_, x)   = (0, x)
        set (Node l r) _ = (1, ElemsCons l (ElemsCons r ElemsNil))

-- | @T@ editor with comboboxes, as an ADTEditor
tEditor1 :: EffRef m => Widget m
tEditor1 = action $ newRef Leaf >>= adtEditor

-- | @T@ editor with checkboxes, given directly
tEditor3 :: EffRef m => Ref m T -> m (Widget m)
tEditor3 r = do
    q <- extRef r tLens (False, (Leaf, Leaf))
    return $ hcat
        [ checkbox $ fstLens `lensMap` q
        , cell (liftM fst $ readRef q) $ \b -> case b of
            False -> empty
            True -> action $ do
                t1 <- tEditor3 $ fstLens . sndLens `lensMap` q
                t2 <- tEditor3 $ sndLens . sndLens `lensMap` q
                return $ vcat [t1, t2]
        ]


