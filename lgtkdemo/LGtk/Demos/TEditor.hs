{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module LGtk.Demos.TEditor where

import Control.Lens hiding (Cons)
import Control.Monad
import LGtk
import LGtk.ADTEditor

-- | Binary tree shapes
data T
    = Leaf
    | Node T T
        deriving (Eq, Show)

-- | Lens for @T@
tLens :: Lens' (Bool, (T, T)) T
tLens = lens get set where
    get (False, _)     = Leaf
    get (True, (l, r)) = Node l r
    set (_, x) Leaf  = (False, x)
    set _ (Node l r) = (True, (l, r))

-- | @ADTLens@ instance for @T@
instance ADTLens T where
    type ADTEls T = Cons T (Cons T Nil)
    adtLens = ([("Leaf",[]),("Node",[0,1])], ElemsCons Leaf (ElemsCons Leaf ElemsNil), Lens_ $ lens get set) where
        get :: (Int, Elems (ADTEls T)) -> T
        get (0, _)     = Leaf
        get (1, ElemsCons l (ElemsCons r ElemsNil)) = Node l r
        set :: (Int, Elems (ADTEls T)) -> T -> (Int, Elems (ADTEls T))
        set (_, x) Leaf  = (0, x)
        set _ (Node l r) = (1, ElemsCons l (ElemsCons r ElemsNil))

-- | @T@ editor with comboboxes, as an ADTEditor
tEditor1 ::  Widget
tEditor1 = join $ newRef Leaf >>= adtEditor

-- | @T@ editor with checkboxes, given directly
tEditor3 ::  Ref T -> Widget
tEditor3 r = do
    q <- extRef r tLens (False, (Leaf, Leaf))
    hcat
        [ checkbox $ _1 `lensMap` q
        , cell (fmap fst $ readRef q) $ \b -> case b of
            False -> empty
            True -> vcat
                [ tEditor3 $ _2 . _1 `lensMap` q
                , tEditor3 $ _2 . _2 `lensMap` q
                ]
        ]


