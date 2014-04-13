{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.ExtRef where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
--import Control.Monad.RWS
--import Control.Monad.Trans.Identity
--import Control.Monad.Operational
import Control.Lens

-- | @m@ has a submonad @(RefStateReader m)@ which is isomorphic to 'Reader'.
class (Monad m, Monad (RefStateReader m)) => MonadRefState m where

    {- | Law: @(RefStateReader m)@  ===  @('Reader' x)@ for some @x@.

    Alternative laws which ensures this isomorphism (@r :: (RefStateReader m a)@ is arbitrary):

     *  @(r >> return ())@ === @return ()@

     *  @liftM2 (,) r r@ === @liftM (\a -> (a, a)) r@

    See also <http://stackoverflow.com/questions/16123588/what-is-this-special-functor-structure-called>
    -}
    type RefStateReader m :: * -> *

    -- | @(RefStateReader m)@ is a submonad of @m@
    liftRefStateReader :: RefStateReader m a -> m a

-- | @RefStateReader (StateT s m) = Reader s@ 
instance Monad m => MonadRefState (StateT s m) where
    type RefStateReader (StateT s m) = Reader s
    liftRefStateReader = gets . runReader


{- |
A reference @(r a)@ is isomorphic to @('Lens' s a)@ for some fixed state @s@.

@r@  ===  @Lens s@
-}
class (MonadRefState (RefState r)) => Reference r where

    {- | @Refmonad r@  ===  @State s@

    Property derived from the 'MonadRefState' instance:

    @RefReader r@ = @RefStateReader (Refmonad r)@  ===  @Reader s@
    -}
    type RefState r :: * -> *

    {- | @readRef@ === @reader . getL@

    Properties derived from the 'MonadRefState' instance:

    @(readRef r >> return ())@ === @return ()@
    -}
    readRef  :: MRef r a -> RefReader r a

    {- | @writeRef r@ === @modify . set r@

    Properties derived from the set-get, get-set and set-set laws for lenses:

     *  @(readRef r >>= writeRef r)@ === @return ()@

     *  @(writeRef r a >> readRef r)@ === @return a@

     *  @(writeRef r a >> writeRef r a')@ === @writeRef r a'@
    -}
    writeRef :: MRef r a -> a -> RefState r ()

    {- | Apply a lens on a reference.

    @lensMap@ === @(.)@
    -}
    lensMap :: Lens' a b -> MRef r a -> MRef r b

    -- | @unitRef@ === @lens (const ()) (const id)@
    unitRef :: MRef r ()

-- | Reference wrapped into a RefReader monad
type MRef r a = RefReader r (r a)

type RefReader m = RefStateReader (RefState m)

infixr 8 `lensMap`


-- | @modRef r f@ === @liftRefStateReader (readRef r) >>= writeRef r . f@
modRef :: Reference r => MRef r a -> (a -> a) -> RefState r ()
r `modRef` f = liftRefStateReader (readRef r) >>= writeRef r . f


{- | Monad for reference creation. Reference creation is not a method
of the 'Reference' type class to make possible to
create the same type of references in multiple monads.

@(Extref m) === (StateT s m)@, where 's' is an extendible state.

For basic usage examples, look into the source of @Control.Monad.ExtRef.Pure.Test@.
-}
class (Monad m, Reference (RefCore m)) => ExtRef m where

    type RefCore m :: * -> *

    -- | @'WriteRef' m@ is a submonad of @m@.
    liftWriteRef :: WriteRef m a -> m a

    {- | Reference creation by extending the state of an existing reference.

    Suppose that @r@ is a reference and @k@ is a lens.

    Law 1: @extRef@ applies @k@ on @r@ backwards, i.e. 
    the result of @(extRef r k a0)@ should behaves exactly as @(lensMap k r)@.

     *  @(liftM (k .) $ extRef r k a0)@ === @return r@

    Law 2: @extRef@ does not change the value of @r@:

     *  @(extRef r k a0 >> readRef r)@ === @(readRef r)@

    Law 3: Proper initialization of newly defined reference with @a0@:

     *  @(extRef r k a0 >>= readRef)@ === @(readRef r >>= set k a0)@
    -}
    extRef :: Ref m b -> Lens' a b -> a -> m (Ref m a)

    {- | @newRef@ extends the state @s@ in an independent way.

    @newRef@ === @extRef unitRef (lens (const ()) (const id))@
    -}
    newRef :: a -> m (Ref m a)
    newRef = extRef unitRef $ lens (const ()) (flip $ const id)

type Ref m a = RefReader (RefCore m) (RefCore m a)

type WriteRef m = RefState (RefCore m)

type ReadRef m = RefReader (RefCore m)

{- | @ReadRef@ lifted to the reference creation class.

Note that we do not lift @WriteRef@ to the reference creation class, which a crucial restriction
in the LGtk interface; this is a feature.
-}
liftReadRef :: ExtRef m => ReadRef m a -> m a
liftReadRef = liftWriteRef . liftRefStateReader

{- | @readRef@ lifted to the reference creation class.

@readRef'@ === @liftReadRef . readRef@
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


-- | This instance is used in the implementation, end users do not need it.
instance (ExtRef m, Monoid w) => ExtRef (WriterT w m) where

    type RefCore (WriterT w m) = RefCore m

    liftWriteRef = lift . liftWriteRef

    extRef x y a = lift $ extRef x y a


-- | Undo-redo state transformation.
undoTr
    :: ExtRef m =>
       (a -> a -> Bool)     -- ^ equality on state
    -> Ref m a             -- ^ reference of state
    ->   m ( ReadRef m (Maybe (WriteRef m ()))   
           , ReadRef m (Maybe (WriteRef m ()))
           )  -- ^ undo and redo actions
undoTr eq r = do
    ku <- extRef r (undoLens eq) ([], [])
    let try f = liftM (liftM (writeRef ku) . f) $ readRef ku
    return (try undo, try redo)
  where
    undo (x: xs@(_:_), ys) = Just (xs, x: ys)
    undo _ = Nothing

    redo (xs, y: ys) = Just (y: xs, ys)
    redo _ = Nothing

undoLens :: (a -> a -> Bool) -> Lens' ([a],[a]) a
undoLens eq = lens get set where
    get = head . fst
    set (x' : xs, ys) x | eq x x' = (x: xs, ys)
    set (xs, _) x = (x : xs, [])


{- | References with inherent equivalence.

-}
class Reference r => EqReference r where

    {- | @hasEffect r f@ returns @False@ iff @(modRef m f)@ === @(return ())@.

    @hasEffect@ is correct only if @eqRef@ is applied on a pure reference (a reference which is a pure lens on the hidden state).

    @hasEffect@ makes defining auto-sensitive buttons easier, for example.
    -}
    hasEffect
        :: MRef r a
        -> (a -> a)
        -> RefReader r Bool


data EqRefCore r a = EqRefCore (r a) (a -> Bool{-changed-})

{- | References with inherent equivalence.

@EqRef r a@ === @RefReader r (exist b . Eq b => (Lens' b a, r b))@

As a reference, @(m :: EqRef r a)@ behaves as

@join $ liftM (uncurry lensMap) m@
-}
type EqRef r a = RefReader r (EqRefCore r a)

{- | @EqRef@ construction.
-}
eqRef :: (Reference r, Eq a) => MRef r a -> EqRef r a
eqRef r = do
    a <- readRef r
    r_ <- r
    return $ EqRefCore r_ $ (/= a)

newEqRef :: (ExtRef m, Eq a) => a -> m (EqRef (RefCore m) a) 
newEqRef = liftM eqRef . newRef

{- | An @EqRef@ is a normal reference if we forget about the equality.

@toRef m@ === @join $ liftM (uncurry lensMap) m@
-}
toRef :: Reference r => EqRef r a -> MRef r a
toRef m = m >>= \(EqRefCore r _) -> return r

instance Reference r => EqReference (EqRefCore r) where
    hasEffect m f = do
        a <- readRef m
        EqRefCore r k <- m
        return $ k $ f a

instance Reference r => Reference (EqRefCore r) where

    type (RefState (EqRefCore r)) = RefState r

    readRef = readRef . toRef

    writeRef = writeRef . toRef

    lensMap l m = do
        a <- readRef m
        EqRefCore r k <- m
        lr <- lensMap l $ return r
        return $ EqRefCore lr $ \b -> k $ set l b a

    unitRef = eqRef unitRef

{-
data CorrRefCore r a = CorrRefCore (r a) (a -> Maybe a{-corrected-})

instance Reference r => Reference (CorrRefCore r) where

    type (RefState (CorrRefCore r)) = RefState r

    readRef = readRef . fromCorrRef

    writeRef = writeRef . fromCorrRef

    lensMap l m = do
        a <- readRef m
        CorrRefCore r k <- m
        lr <- lensMap l $ return r
        return $ CorrRefCore lr $ \b -> fmap (^. l) $ k $ set l b a

    unitRef = corrRef unitRef

fromCorrRef m = m >>= \(CorrRefCore r _) -> return r

corrRef r = do
    a <- readRef r
    r_ <- r
    return $ CorrRefCore r_ $ \a' -> Nothing
-}
