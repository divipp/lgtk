{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.Monad.ExtRef
    ( module Data.Lens.Common

    -- * Restricted monads
    , HasReadPart (..)

    -- * Reference class
    , Reference (..)

    -- * Ref construction class
    , ExtRef (..)

    -- * Derived constructs
    , ReadRef
    , WriteRef
    , modRef
    , liftReadRef
    , readRef'
    , undoTr
    , memoRead
    , memoWrite

    -- * References with equation
    , EqRef_ (..)
    , EqRef (..)
    , toRef
    , eqRef

    -- * Auxiliary definitions
    , Morph
    , MorphD (..)
    , MonadIO' (..)

    -- * Auxiliary lens definitions
    , listLens
    , maybeLens
    , showLens

    -- * Re-exported
    , (.)
    , id
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Trans.Identity
import Control.Category
import Data.Maybe
import Data.Lens.Common
import Prelude hiding ((.), id)

import Control.Monad.Restricted

{- |
A reference @(r a)@ is isomorphic to @('Lens' s a)@ for some fixed state @s@.

@r@  ===  @Lens s@
-}
class (HasReadPart (RefMonad r)) => Reference r where

    {- | @Refmonad r@  ===  @State s@

    Property derived from the 'HasReadPart' instance:

    @ReadPart (Refmonad r)@  ===  @Reader s@
    -}
    type RefMonad r :: * -> *

    {- | @readRef@ === @reader . getL@

    Property derived from the set-get law for lenses:

    @(readRef r >>= writeRef r)@ === @return ()@

    Properties derived from the 'HasReadPart' instance:

    @(readRef r >> return ())@ === @return ()@
    -}
    readRef  :: r a -> ReadPart (RefMonad r) a

    {- | @writeRef r@ === @modify . setL r@

    Properties derived from the get-set and set-set laws for lenses:

     *  @(writeRef r a >> readRef r)@ === @return a@

     *  @(writeRef r a >> writeRef r a')@ === @writeRef r a'@
    -}
    writeRef :: r a -> a -> RefMonad r ()

    {- | Apply a lens on a reference.

    @lensMap@ === @(.)@
    -}
    lensMap :: Lens a b -> r a -> r b

    {- | @joinRef@ makes possible to define dynamic references, i.e. references which depends on
    values of other references.
    It is not possible to create new reference dynamically with @joinRef@; for that, see 'onChange'.

    @joinRef@ === @Lens . join . (runLens .) . runReader@
    -}
    joinRef :: ReadPart (RefMonad r) (r a) -> r a

    -- | @unitRef@ === @lens (const ()) (const id)@
    unitRef :: r ()

infixr 8 `lensMap`

-- | @modRef r f@ === @liftReadPart (readRef r) >>= writeRef r . f@
modRef :: Reference r => r a -> (a -> a) -> RefMonad r ()
r `modRef` f = liftReadPart (readRef r) >>= writeRef r . f

type WriteRef m = RefMonad (Ref m)

type ReadRef m = ReadPart (RefMonad (Ref m))

{- | @ReadRef@ lifted to the reference creation class.

Note that we do not lift @WriteRef@ to the reference creation class, which a crucial restriction
in the LGtk interface; this is a feature.
-}
liftReadRef :: ExtRef m => Morph (ReadRef m) m
liftReadRef = liftWriteRef . liftReadPart

{- | @readRef@ lifted to the reference creation class.
-}
readRef' :: ExtRef m => Ref m a -> m a
readRef' = liftReadRef . readRef

{- | Lazy monadic evaluation.
In case of @y <- memoRead x@, invoking @y@ will invoke @x@ at most once.

Laws:

 *  @(memoRead x >> return ())@ === @return ()@

 *  @(memoRead x >>= id)@ === @x@

 *  @(memoRead x >>= \y -> liftM2 (,) y y)@ === @liftM (\a -> (a, a)) y@

 *  @(memoRead x >>= \y -> liftM3 (,) y y y)@ === @liftM (\a -> (a, a, a)) y@

 *  ...
-}
memoRead :: ExtRef m => m a -> m (m a)
memoRead g = do
    s <- newRef Nothing
    return $ readRef' s >>= \x -> case x of
        Just a -> return a
        _ -> g >>= \a -> do
            liftWriteRef $ writeRef s $ Just a
            return a

memoWrite :: (ExtRef m, Eq b) => (b -> m a) -> m (b -> m a)
memoWrite g = do
    s <- newRef Nothing
    return $ \b -> readRef' s >>= \x -> case x of
        Just (b', a) | b' == b -> return a
        _ -> g b >>= \a -> do
            liftWriteRef $ writeRef s $ Just (b, a)
            return a


{- | Monad for reference creation. Reference creation is not a method
of the 'Reference' type class to make possible to
create the same type of references in multiple monads.

@(Extref m) === (StateT s m)@, where 's' is an extendible state.

For basic usage examples, look into the source of @Control.Monad.ExtRef.Pure.Test@.
-}
class (Monad m, Reference (Ref m)) => ExtRef m where

    type Ref m :: * -> *

    -- | @'WriteRef' m@ is a submonad of @m@.
    liftWriteRef :: Morph (WriteRef m) m

    {- | Reference creation by extending the state of an existing reference.

    Suppose that @r@ is a reference and @k@ is a lens.

    Law 1: @extRef@ applies @k@ on @r@ backwards, i.e. 
    the result of @(extRef r k a0)@ should behaves exactly as @(lensMap k r)@.

     *  @(liftM (k .) $ extRef r k a0)@ === @return r@

    Law 2: @extRef@ does not change the value of @r@:

     *  @(extRef r k a0 >> readRef r)@ === @(readRef r)@

    Law 3: Proper initialization of newly defined reference with @a0@:

     *  @(extRef r k a0 >>= readRef)@ === @(readRef r >>= setL k a0)@
    -}
    extRef :: Ref m b -> Lens a b -> a -> m (Ref m a)

    {- | @newRef@ extends the state @s@ in an independent way.

    @newRef@ === @extRef unitRef (lens (const ()) (const id))@
    -}
    newRef :: a -> m (Ref m a)
    newRef = extRef unitRef $ lens (const ()) (const id)

-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m, Monoid w) => ExtRef (WriterT w m) where

    type Ref (WriterT w m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef x y a = lift $ extRef x y a
{-
instance (ExtRef m) => ExtRef (ReaderT s m) where

    type Ref (ReaderT s m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef r k a = lift $ extRef r k a
-}

-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m) => ExtRef (IdentityT m) where

    type Ref (IdentityT m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef r k a = lift $ extRef r k a

-- | This instance is used in the implementation, the end users do not need it.
instance (ExtRef m, Monoid w) => ExtRef (RWST r w s m) where

    type Ref (RWST r w s m) = Ref m

    liftWriteRef = lift . liftWriteRef

    extRef r k a = lift $ extRef r k a


-- | Undo-redo state transformation.
undoTr
    :: ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a             -- ^ reference of state
    ->   m ( ReadRef m (Maybe (WriteRef m ()))   
           , ReadRef m (Maybe (WriteRef m ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extRef r undoLens ([], [])
    let try f = liftM (liftM (writeRef ku) . f) $ readRef ku
    return (try undo, try redo)
  where
    undoLens = lens get set where
        get = head . fst
        set x (x' : xs, ys) | eq x x' = (x: xs, ys)
        set x (xs, _) = (x : xs, [])

    undo (x: xs@(_:_), ys) = Just (xs, x: ys)
    undo _ = Nothing

    redo (xs, y: ys) = Just (y: xs, ys)
    redo _ = Nothing


data EqRef_ r a = forall b . Eq b => EqRef_ (r b) (Lens b a)

newtype EqRef r a = EqRef { runEqRef :: ReadPart (RefMonad r) (EqRef_ r a) }

-- instance Reference r => Reference (EqRef r) where

toRef :: Reference r => EqRef r a -> r a
toRef (EqRef m) = joinRef $ liftM (\(EqRef_ r k) -> k `lensMap` r) m

eqRef :: (Reference r, Eq a) => r a -> EqRef r a
eqRef r = EqRef $ return $ EqRef_ r id

instance Reference r => Reference (EqRef r) where

    type (RefMonad (EqRef r)) = RefMonad r

    readRef = readRef . toRef

    writeRef = writeRef . toRef

    lensMap l (EqRef m) = EqRef $ m >>= \(EqRef_ r k) -> return $ EqRef_ r $ l . k

    joinRef = EqRef . join . liftM runEqRef

    unitRef = eqRef unitRef




showLens :: (Show a, Read a) => Lens a String
showLens = lens show $ \s def -> maybe def fst $ listToMaybe $ reads s

listLens :: Lens (Bool, (a, [a])) [a]
listLens = lens get set where
    get (False, _) = []
    get (True, (l, r)) = l: r
    set [] (_, x) = (False, x)
    set (l: r) _ = (True, (l, r))


maybeLens :: Lens (Bool, a) (Maybe a)
maybeLens = lens (\(b,a) -> if b then Just a else Nothing)
              (\x (_,a) -> maybe (False, a) (\a' -> (True, a')) x)


