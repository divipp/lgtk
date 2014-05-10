{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK prune #-}
module Data.LensRef
    (
    -- * Core

    -- ** References
      RefClass (..)
    , RefSimple
    , RefWriterOf
    , RefWriterSimple

    , MonadRefReader (..)
    , MonadRefWriter (..)

    -- ** Reference creation
    , MonadRefCreator (..)
    , Ref
    , RefReader
    , RefWriter

    -- ** Dynamic networks
    , MonadRegister (..)
    , RegisteredCallbackCommand (..)

    -- ** Other
    , MonadMemo (..)

    -- * Derived constructs
    , modRef
    , registerCallbackSimple
    , onChangeEffect
    , iReallyWantToModify
--    , undoTr

    , EqRefClass (..)
    , EqRefSimple, EqRef
    , hasEffect
    , toEqRef
    , fromEqRef
    , newEqRef
{-
    , CorrRef
    , corrRef
    , fromCorrRef
    , correction
-}
    ) where


import Control.Monad (liftM, join)
import Control.Lens (Lens', lens, set)

--------------------------------


{- |
Type class for references which can be joined and on which lenses can be applied.

The join operation is 'join' from "Control.Monad":
If @(r :: RefReaderSimple r (RefSimple r a))@ then @(join r :: RefSimple r a)@.
This is possible because reference operations work with @(RefReaderSimple r (r a))@ instead
of just @(r a)@. For more compact type signatures, @(RefReaderSimple r (r a))@ is called @(RefSimple r a)@.
-}
class (MonadRefWriter (RefWriterSimple r), MonadRefReader (RefReaderSimple r), RefReader (RefReaderSimple r) ~ RefReaderSimple r) => RefClass r where

    {- | unit reference

    Laws:

    prop> writeRef unitRef () >> m  =  m
    -}
    unitRef :: RefSimple r ()

    {- | Apply a lens on a reference.
    -}
    lensMap :: Lens' a b -> RefSimple r a -> RefSimple r b

    {- | Associated reader monad.

    @(RefReaderSimple m)@ is ismoroprhic to @('Reader' x)@ for some @x@.
    Laws which ensures this isomorphism (@(r :: RefReaderSimple m a)@ is arbitrary):

    prop> r >> return ()  =  return ()
    prop> liftM2 (,) r r  =  liftM (\a -> (a, a)) r

    See also <http://stackoverflow.com/questions/16123588/what-is-this-special-functor-structure-called>
    -}
    type RefReaderSimple r :: * -> *

    {- | Reference read.

    Laws:

    prop> readRef r >> return ())  =  return ()
    -}
    readRefSimple  :: RefSimple r a -> RefReaderSimple r a

    {- | Reference write.

    Properties derived from the set-get, get-set and set-set laws for lenses:

    prop> readRef r >>= writeRef r  =  return ()
    prop> writeRef r a >> readRef r)  =  return a
    prop> writeRef r a >> writeRef r a')  =  writeRef r a'
    -}
    writeRefSimple :: RefSimple r a -> a -> RefWriterSimple r ()


data family RefWriterOf (m :: * -> *) a :: *

{- |
There are two associated types of a reference, 'RefReaderSimple' and 'RefWriterSimple' which determines each-other.
This is implemented by putting only 'RefReaderSimple' into the 'RefClass' class and
adding a @RefWriterOf@ data family outside of 'RefClass'.

@RefWriterOf@ is hidden from the documentation because you never need it explicitly.
-}
type RefWriterSimple m = RefWriterOf (RefReaderSimple m)

-- | Reference wrapped into a RefReaderSimple monad. See the documentation of 'RefClass'.
type RefSimple r a = RefReaderSimple r (r a)

infixr 8 `lensMap`

-- | TODO
class Monad m => MonadRefReader m where

    -- | Base reference associated to the reference reader monad
    type BaseRef m :: * -> *

    {- | @RefReader@ lifted to the reference creation class.

    Note that we do not lift @RefWriter@ to the reference creation class, which a crucial restriction
    in the LGtk interface; this is a feature.
    -}
    liftRefReader :: RefReader m a -> m a

    {- | @readRef@ lifted to the reference creation class.

    @readRef@ === @liftRefReader . readRefSimple@
    -}
    readRef :: (RefClass r, RefReader m ~ RefReaderSimple r) => RefSimple r a -> m a
    readRef = liftRefReader . readRefSimple


-- | TODO
type RefReader m = RefReaderSimple (BaseRef m)

-- | TODO
type RefWriter m = RefWriterSimple (BaseRef m)

-- | TODO
type Ref m a = RefSimple (BaseRef m) a



-- | TODO
class MonadRefReader m => MonadRefWriter m where

    liftRefWriter :: RefWriter m a -> m a

    writeRef :: (RefClass r, RefReaderSimple r ~ RefReader m) => RefSimple r a -> a -> m ()
    writeRef r = liftRefWriter . writeRefSimple r






{- | Monad for reference creation. Reference creation is not a method
of the 'RefClass' type class to make possible to
create the same type of references in multiple monads.

@(Extref m) === (StateT s m)@, where 's' is an extendible state.

For basic usage examples, look into the source of @Data.LensRef.Pure.Test@.
-}
class (Monad m, RefClass (BaseRef m), MonadRefReader m, MonadMemo m) => MonadRefCreator m where

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


-- | TODO
class Monad m => MonadMemo m where
    {- | Lazy monadic evaluation.
    In case of @y <- memoRead x@, invoking @y@ will invoke @x@ at most once.

    Laws:

     *  @(memoRead x >> return ())@ === @return ()@

     *  @(memoRead x >>= id)@ === @x@

     *  @(memoRead x >>= \y -> liftM2 (,) y y)@ === @liftM (\a -> (a, a)) y@

     *  @(memoRead x >>= \y -> liftM3 (,) y y y)@ === @liftM (\a -> (a, a, a)) y@

     *  ...
    -}
    memoRead :: m a -> m (m a)
{-
    memoWrite :: Eq b => (b -> m a) -> m (b -> m a)

    future :: (RefReader m a -> m a) -> m a
-}

-- | Monad for dynamic actions
class (MonadRefCreator m, Monad (EffectM m), MonadRefWriter (Modifier m), MonadRefCreator (Modifier m), BaseRef (Modifier m) ~ BaseRef m) => MonadRegister m where

    type EffectM m :: * -> *

    liftEffectM :: EffectM m a -> m a

    {- |
    Let @r@ be an effectless action (@RefReader@ guarantees this).

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
    onChangeAcc
        :: Eq b
        => RefReader m b
        -> b -> (b -> c)
        -> (b -> b -> c -> m (c -> m c))
        -> m (RefReader m c)

    onChange :: Eq a => RefReader m a -> (a -> m (m b)) -> m (RefReader m b)
    onChange r f = onChangeAcc r undefined undefined $ \b _ _ -> liftM (\x _ -> x) $ f b

    onChangeSimple :: Eq a => RefReader m a -> (a -> m b) -> m (RefReader m b)
    onChangeSimple r f = onChange r $ return . f

    data Modifier m a :: *

    liftModifier :: m a -> Modifier m a

    registerCallback :: Functor f => f (Modifier m ()) -> (RegisteredCallbackCommand -> EffectM m ()) -> m (f (EffectM m ()))

-- | TODO
data RegisteredCallbackCommand = Kill | Block | Unblock deriving (Eq, Ord, Show)





-------------- derived constructs


-- | TODO
registerCallbackSimple :: MonadRegister m => Modifier m () -> (RegisteredCallbackCommand -> EffectM m ()) -> m (EffectM m ())
registerCallbackSimple m c = do
    f <- registerCallback (const m) c
    return $ f ()

-- | TODO
onChangeEffect  :: (MonadRegister m, Eq a) => RefReader m a -> (a -> EffectM m b) -> m (RefReader m b)
onChangeEffect r f = onChangeSimple r $ liftEffectM . f

-- | @modRef r f@ === @readRef r >>= writeRef r . f@
modRef :: (MonadRefWriter m, RefClass r, RefReaderSimple r ~ RefReader m) => RefSimple r a -> (a -> a) -> m ()
r `modRef` f = readRef r >>= writeRef r . f


-- | TODO
iReallyWantToModify :: MonadRegister m => Modifier m () -> m ()
iReallyWantToModify r = do
    x <- registerCallbackSimple r $ const $ return ()
    liftEffectM x




{- | RefClasss with inherent equivalence.

-}
class RefClass r => EqRefClass r where
    valueIsChanging :: RefSimple r a -> RefReaderSimple r (a -> Bool)

{- | @hasEffect r f@ returns @False@ iff @(modRef m f)@ === @(return ())@.

@hasEffect@ is correct only if @toEqRef@ is applied on a pure reference (a reference which is a pure lens on the hidden state).

@hasEffect@ makes defining auto-sensitive buttons easier, for example.
-}
hasEffect
    :: EqRefClass r
    => RefSimple r a
    -> (a -> a)
    -> RefReaderSimple r Bool
hasEffect r f = do
    a <- readRef r
    ch <- valueIsChanging r
    return $ ch $ f a


-- | TODO
data EqRefCore r a = EqRefCore (r a) (a -> Bool{-changed-})

{- | RefClasss with inherent equivalence.

@EqRefSimple r a@ === @RefReaderSimple r (exist b . Eq b => (Lens' b a, r b))@

As a reference, @(m :: EqRefSimple r a)@ behaves as

@join $ liftM (uncurry lensMap) m@
-}
type EqRefSimple r a = RefReaderSimple r (EqRefCore r a)

-- | TODO
type EqRef m a = EqRefSimple (BaseRef m) a

{- | @EqRefSimple@ construction.
-}
toEqRef :: (RefClass r, Eq a) => RefSimple r a -> EqRefSimple r a
toEqRef r = do
    a <- readRef r
    r_ <- r
    return $ EqRefCore r_ $ (/= a)

-- | TODO
newEqRef :: (MonadRefCreator m, Eq a) => a -> m (EqRef m a) 
newEqRef = liftM toEqRef . newRef

{- | An @EqRefSimple@ is a normal reference if we forget about the equality.

@fromEqRef m@ === @join $ liftM (uncurry lensMap) m@
-}
fromEqRef :: RefClass r => EqRefSimple r a -> RefSimple r a
fromEqRef m = m >>= \(EqRefCore r _) -> return r

instance RefClass r => EqRefClass (EqRefCore r) where
    valueIsChanging m = do
        EqRefCore _r k <- m
        return k

instance RefClass r => RefClass (EqRefCore r) where

    type (RefReaderSimple (EqRefCore r)) = RefReaderSimple r

    readRefSimple = readRef . fromEqRef

    writeRefSimple = writeRefSimple . fromEqRef

    lensMap l m = do
        a <- readRef m
        EqRefCore r k <- m
        lr <- lensMap l $ return r
        return $ EqRefCore lr $ \b -> k $ set l b a

    unitRef = toEqRef unitRef

{-
data CorrBaseRef r a = CorrBaseRef (r a) (a -> Maybe a{-corrected-})

type CorrRef r a = RefReaderSimple r (CorrBaseRef r a)

instance RefClass r => RefClass (CorrBaseRef r) where

    type (RefReaderSimple (CorrBaseRef r)) = RefReaderSimple r

    readRef = readRef . fromCorrRef

    writeRefSimple = writeRefSimple . fromCorrRef

    lensMap l m = do
        a <- readRef m
        CorrBaseRef r k <- m
        lr <- lensMap l $ return r
        return $ CorrBaseRef lr $ \b -> fmap (^. l) $ k $ set l b a

    unitRef = corrRef (const Nothing) unitRef

fromCorrRef :: RefClass r => CorrRef r a -> RefSimple r a
fromCorrRef m = m >>= \(CorrBaseRef r _) -> return r

corrRef :: RefClass r => (a -> Maybe a) -> RefSimple r a -> CorrRef r a
corrRef f r = do
    r_ <- r
    return $ CorrBaseRef r_ f

correction :: RefClass r => CorrRef r a -> RefReaderSimple r (a -> Maybe a)
correction r = do
    CorrBaseRef _ f <- r
    return f
-}


