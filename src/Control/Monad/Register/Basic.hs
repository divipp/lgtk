{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Register.Basic
    ( Register
    , evalRegister
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Prelude hiding ((.), id)

import Control.Monad.Restricted
import Control.Monad.Register
import Control.Monad.ExtRef

data RegisterState m = RegisterState
    { sendEvent :: m () -> m ()
    , newRef' :: forall a . a -> m (MorphD (State a) m)
    }

newtype MM a = MM { runMM :: a () }

instance Monad m => Monoid (MM m) where
    mempty = MM $ return ()
    MM a `mappend` MM b = MM $ a >> b

newtype Register m a = Register { runRegister :: ReaderT (RegisterState m) (WriterT (MM m, Command -> MM m) m) a }
    deriving (Functor, Monad, MonadIO)

instance MonadTrans Register where
    lift = Register . lift . lift

instance ExtRef m => ExtRef (Register m) where

    type Ref (Register m) = Ref m

    liftWriteRef = Register . liftWriteRef

    extRef r k a = Register $ extRef r k a


instance (Monad m) => MonadRegister (Register m) where

    type EffectM (Register m) = m

    liftEffectM = lift

    toReceive_ r int = do
        rr <- Register ask
        unreg <- liftEffectM $ int $ sendEvent rr . r
        Register $ tell $ t2 unreg

    toSend_ bb rb fb = do
        rr <- Register ask
        memoref <- lift $ newRef' rr (const $ return (), const $ return (), [])  -- unreg action, memo table, first item is the newest
        Register $ tell $ t1 $ do
            b <- rb
            let doit c (s1, ureg1) = do 
                    (s2_, ureg2_) <- execWriterT $ runReaderT (runRegister c) rr
                    let s2 = runMM s2_
                        ureg2 = runMM . ureg2_
                    runMorphD memoref $ state $ \(_, _, memo) -> (,) () (ureg1, ureg2, (b, (c, s1, s2, ureg1, ureg2)) : if bb then filter ((/= b) . fst) memo else [])
                    s1 >> s2
            join $ runMorphD memoref $ gets $ \memo -> case memo of
                (_, _, ((b', (_, s1, s2, _, _)): _)) | b' == b -> s1 >> s2
                (ur1, ur2, memo) -> do
                  ur1 $ if bb then Block else Kill
                  ur2 Kill
                  case (bb, filter ((== b) . fst) memo) of
                    (True, (_, (c, s1, _, ureg1, ureg2)): _) -> ureg1 Unblock >> doit c (s1, ureg1)
                    _ -> do
                        (c, s1_) <- runWriterT $ runReaderT (runRegister $ fb b) rr
                        let s1 = (runMM $ fst s1_, runMM . snd s1_)
                        doit c s1

t1 m = (MM m, mempty)
t2 m = (mempty, MM . m)

-- | evaluation with postponed actions
evalRegister :: (Monad k)
    => (forall a . a -> k (MorphD (State a) k))
    -> ((k () -> k ()) -> Register k a)
    -> (k () -> k ())
    -> k a
{-
evalRegister :: (HasReadPart n, ExtRef m, n ~ RefMonad (Ref m), MonadIO m, Monad k)
    => (forall a . a -> k (MorphD (State a) k))
    -> Morph n k
    -> Morph k m
    -> Morph m k
    -> (forall t . (MonadTrans t, MonadRegister (t m), PureM (t m) ~ n, EffectM (t m) ~ k, MonadIO (t m)
       , ExtRef (t m), PureM (t m) ~ WriteRef (t m)) => (k () -> k ()) -> t m a)
    -> (k () -> k ())
    -> m a
-}
evalRegister newRef' f ch = do
    let liftIO = id
    post <- liftIO $ newRef' $ return ()
    let (Register m) = f $ runMorphD post . modify . flip (>>)
    vx <- liftIO $ newRef' $ error "evalRegister"
    (a, reg) <- runWriterT $ runReaderT m $ RegisterState
        (\m -> ch $ do
                m
                join (runMorphD vx get)
                join (runMorphD post $ state $ \m -> (m, return ()))
        )
        (liftIO . newRef')
    liftIO $ runMorphD vx $ put $ runMM $ fst reg
    liftIO $ runMM $ fst reg        -- needed?
    return a



