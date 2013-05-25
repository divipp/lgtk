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

data RegisterState m k = RegisterState
    { sendEvent :: k () -> k ()
    , morph :: MorphD m k
    , morphK :: MorphD k m
    , newRef' :: forall a . a -> k (MorphD (State a) k)
    }

newtype MM a = MM { runMM :: a () }

instance Monad m => Monoid (MM m) where
    mempty = MM $ return ()
    MM a `mappend` MM b = MM $ a >> b

newtype Register k m a = Register { runRegister :: ReaderT (RegisterState m k) (WriterT (MM k, Command -> MM k) m) a }
    deriving (Functor, Monad, MonadIO)

instance (Monad k) => MonadTrans (Register k) where
    lift = Register . lift . lift

instance (ExtRef m, Monad k) => ExtRef (Register k m) where

    type Ref (Register k m) = Ref m

    liftWriteRef = Register . liftWriteRef

    extRef r k a = Register $ extRef r k a


instance (Monad m, Monad k) => MonadRegister (Register k m) where

    type PureM (Register k m) = m
    type EffectM (Register k m) = k

    liftEffectM m = do
        rr <- Register $ ask
        lift $ runMorphD (morphK rr) m

    toReceive_ r int = do
        rr <- Register ask
        unreg <- liftEffectM $ int $ sendEvent rr . runMorphD (morph rr) . r
        Register $ tell $ t2 unreg

    toSend_ bb rb fb = do
        rr <- Register ask
        memoref <- liftEffectM $ newRef' rr (const $ return (), const $ return (), [])  -- unreg action, memo table, first item is the newest
        Register $ tell $ t1 $ do
            b <- runMorphD (morph rr) rb
            let doit c (s1, ureg1) = do 
                    (s2_, ureg2_) <- runMorphD (morph rr) $ execWriterT $ runReaderT (runRegister c) rr
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
                        (c, s1_) <- runMorphD (morph rr) $ runWriterT $ runReaderT (runRegister $ fb b) rr
                        let s1 = (runMM $ fst s1_, runMM . snd s1_)
                        doit c s1

t1 m = (MM m, mempty)
t2 m = (mempty, MM . m)

-- | evaluation with postponed actions
evalRegister :: (Monad m, Monad k)
    => (forall a . a -> k (MorphD (State a) k))
    -> Morph k m
    -> Morph m k
    -> ((k () -> k ()) -> Register k m a)
    -> (k () -> k ())
    -> m a
    post <- liftIO $ newRef' $ return ()
    let (Register m) = f $ runMorphD post . modify . flip (>>)
    vx <- liftIO $ newRef' $ error "evalRegister"
    (a, reg) <- runWriterT $ runReaderT m $ RegisterState
        (\m -> ch $ m
          >> join (runMorphD vx get) >> join (runMorphD post $ state $ \m -> (m, return ())))
        (MorphD morph)
        (MorphD liftIO)
        newRef'
    liftIO $ runMorphD vx $ put $ runMM $ fst reg
    liftIO $ runMM $ fst reg        -- needed?
    return a

