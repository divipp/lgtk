{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.ExtRef where

import Control.Monad (liftM)
import Control.Lens (Lens', lens, set, (^.))

--------------------------------

-- | @m@ has a submonad @(RefState m)@ which is isomorphic to 'Reader'.
class (Monad m, Monad (RefState m)) => MonadRefReader m where

    {- | Law: @(RefState m)@  ===  @('Reader' x)@ for some @x@.

    Alternative laws which ensures this isomorphism (@r :: (RefState m a)@ is arbitrary):

     *  @(r >> return ())@ === @return ()@

     *  @liftM2 (,) r r@ === @liftM (\a -> (a, a)) r@

    See also <http://stackoverflow.com/questions/16123588/what-is-this-special-functor-structure-called>
    -}
    data RefState m a :: *

    -- | @m@ is a submonad of @(RefState m)@
    liftRefStateReader :: m a -> RefState m a



{- |
A reference @(r a)@ is isomorphic to @('Lens' s a)@ for some fixed state @s@.

@r@  ===  @Lens s@
-}
class (MonadRefReader (RefReader r)) => Reference r where

    {- | @Refmonad r@  ===  @State s@

    Property derived from the 'MonadRefReader' instance:

    @RefReader r@ = @RefState (Refmonad r)@  ===  @Reader s@
    -}
    type RefReader r :: * -> *

    {- | @readRef@ === @reader . getL@

    Properties derived from the 'MonadRefReader' instance:

    @(readRef r >> return ())@ === @return ()@
    -}
    readRef  :: MRef r a -> RefReader r a

    {- | @writeRef r@ === @modify . set r@

    Properties derived from the set-get, get-set and set-set laws for lenses:

     *  @(readRef r >>= writeRef r)@ === @return ()@

     *  @(writeRef r a >> readRef r)@ === @return a@

     *  @(writeRef r a >> writeRef r a')@ === @writeRef r a'@
    -}
    writeRef :: MRef r a -> a -> RefState (RefReader r) ()

    {- | Apply a lens on a reference.

    @lensMap@ === @(.)@
    -}
    lensMap :: Lens' a b -> MRef r a -> MRef r b

    -- | @unitRef@ === @lens (const ()) (const id)@
    unitRef :: MRef r ()

-- | Reference wrapped into a RefReader monad
type MRef r a = RefReader r (r a)

infixr 8 `lensMap`


{- | Monad for reference creation. Reference creation is not a method
of the 'Reference' type class to make possible to
create the same type of references in multiple monads.

@(Extref m) === (StateT s m)@, where 's' is an extendible state.

For basic usage examples, look into the source of @Control.Monad.ExtRef.Pure.Test@.
-}
class (Monad m, Reference (RefCore m)) => ExtRef m where

    type RefCore m :: * -> *

    {- | @ReadRef@ lifted to the reference creation class.

    Note that we do not lift @WriteRef@ to the reference creation class, which a crucial restriction
    in the LGtk interface; this is a feature.
    -}
    liftReadRef :: ExtRef m => ReadRef m a -> m a

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

    memoWrite :: (ExtRef m, Eq b) => (b -> m a) -> m (b -> m a)


type Ref m a = ReadRef m (RefCore m a)

type ReadRef m = RefReader (RefCore m)

type WriteRef m = RefState (ReadRef m)


-- | Monad for dynamic actions
class (ExtRef m, ExtRef (Modifier m), RefCore (Modifier m) ~ RefCore m) => EffRef m where

    type EffectM m :: * -> *

    data Modifier m a :: *

    liftEffectM :: EffectM m a -> m a

    liftModifier :: m a -> Modifier m a

    liftWriteRef' :: WriteRef m a -> Modifier m a

    {- |
    Let @r@ be an effectless action (@ReadRef@ guarantees this).

    @(onChange init r fmm)@ has the following effect:

    Whenever the value of @r@ changes (with respect to the given equality),
    @fmm@ is called with the new value @a@.
    The value of the @(fmm a)@ action is memoized,
    but the memoized value is run again and again.

    The boolean parameter @init@ tells whether the action should
    be run in the beginning or not.

    For example, let @(k :: a -> m b)@ and @(h :: b -> m ())@,
    and suppose that @r@ will have values @a1@, @a2@, @a3@ = @a1@, @a4@ = @a2@.

    @onChange True r $ \\a -> k a >>= return . h@

    has the effect

    @k a1 >>= \\b1 -> h b1 >> k a2 >>= \\b2 -> h b2 >> h b1 >> h b2@

    and

    @onChange False r $ \\a -> k a >>= return . h@

    has the effect

    @k a2 >>= \\b2 -> h b2 >> k a1 >>= \\b1 -> h b1 >> h b2@
    -}
    onChange_
        :: Eq b
        => ReadRef m b
        -> b -> (b -> c)
        -> (b -> b -> c -> m (c -> m c))
        -> m (ReadRef m c)

    toReceive :: Functor f => f (Modifier m ()) -> (Command -> EffectM m ()) -> m (f (EffectM m ()))

data Command = Kill | Block | Unblock deriving (Eq, Ord, Show)





-------------- derived constructs


{- | @readRef@ lifted to the reference creation class.

@readRef'@ === @liftReadRef . readRef@
-}
readRef' :: ExtRef m => Ref m a -> m a
readRef' = liftReadRef . readRef



{- | References with inherent equivalence.

-}
class Reference r => EqReference r where
    valueIsChanging :: MRef r a -> RefReader r (a -> Bool)

{- | @hasEffect r f@ returns @False@ iff @(modRef m f)@ === @(return ())@.

@hasEffect@ is correct only if @eqRef@ is applied on a pure reference (a reference which is a pure lens on the hidden state).

@hasEffect@ makes defining auto-sensitive buttons easier, for example.
-}
hasEffect
    :: EqReference r
    => MRef r a
    -> (a -> a)
    -> RefReader r Bool
hasEffect r f = do
    a <- readRef r
    ch <- valueIsChanging r
    return $ ch $ f a


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
    valueIsChanging m = do
        EqRefCore _r k <- m
        return k

instance Reference r => Reference (EqRefCore r) where

    type (RefReader (EqRefCore r)) = RefReader r

    readRef = readRef . toRef

    writeRef = writeRef . toRef

    lensMap l m = do
        a <- readRef m
        EqRefCore r k <- m
        lr <- lensMap l $ return r
        return $ EqRefCore lr $ \b -> k $ set l b a

    unitRef = eqRef unitRef


data CorrRefCore r a = CorrRefCore (r a) (a -> Maybe a{-corrected-})

type CorrRef r a = RefReader r (CorrRefCore r a)

instance Reference r => Reference (CorrRefCore r) where

    type (RefReader (CorrRefCore r)) = RefReader r

    readRef = readRef . fromCorrRef

    writeRef = writeRef . fromCorrRef

    lensMap l m = do
        a <- readRef m
        CorrRefCore r k <- m
        lr <- lensMap l $ return r
        return $ CorrRefCore lr $ \b -> fmap (^. l) $ k $ set l b a

    unitRef = corrRef (const Nothing) unitRef

fromCorrRef :: Reference r => CorrRef r a -> MRef r a
fromCorrRef m = m >>= \(CorrRefCore r _) -> return r

corrRef :: Reference r => (a -> Maybe a) -> MRef r a -> CorrRef r a
corrRef f r = do
    r_ <- r
    return $ CorrRefCore r_ f

correction :: Reference r => CorrRef r a -> RefReader r (a -> Maybe a)
correction r = do
    CorrRefCore _ f <- r
    return f

----------------

rEffect  :: (EffRef m, Eq a) => ReadRef m a -> (a -> EffectM m b) -> m (ReadRef m b)
rEffect r f = onChange r $ return . liftEffectM . f

onChange :: (EffRef m, Eq a) => ReadRef m a -> (a -> m (m b)) -> m (ReadRef m b)
onChange r f = onChange_ r undefined undefined $ \b _ _ -> liftM (\x _ -> x) $ f b

writeRef' :: (EffRef m, Reference r, RefReader r ~ RefReader (RefCore m)) => MRef r a -> a -> Modifier m ()
writeRef' r a = liftWriteRef' $ writeRef r a

-- | @modRef r f@ === @liftRefStateReader (readRef r) >>= writeRef r . f@
--modRef :: Reference r => MRef r a -> (a -> a) -> RefStateReader (RefReader r) ()
r `modRef'` f = liftRefStateReader' (readRef r) >>= writeRef' r . f

liftRefStateReader' :: EffRef m => ReadRef m a -> Modifier m a
liftRefStateReader' r = liftWriteRef' $ liftRefStateReader r

action' m = liftModifier $ liftEffectM m




