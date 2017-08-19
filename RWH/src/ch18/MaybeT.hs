{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ch18.MaybeT
where

import qualified Control.Monad as M
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Trans as MT


newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }


instance Functor m => Functor (MaybeT m) where
  -- fmap :: (a -> b) -> (MaybeT m) a -> (MaybeT m) b
  fmap f m =
    MaybeT $ fmap (maybe Nothing (Just . f)) (runMaybeT m)


instance (Functor m, Monad m) => Applicative (MaybeT m) where
  -- pure :: a -> MaybeT m a
  pure x =
    MaybeT $ pure (Just x)

  -- (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  mtf <*> mtx =
    MaybeT $ runMaybeT mtf >>= maybe (pure Nothing) (\f -> runMaybeT $ fmap f mtx)


instance (Monad m) => Monad (MaybeT m) where
  -- return :: (Monad m) => a -> MaybeT m a
  return x =
    MaybeT $ return (Just x)

  -- m >>= g :: (Monad m) => (MaybeT m) a -> (a -> (MaybeT m) b) -> (MaybeT m) b
  m >>= g =
    MaybeT $ runMaybeT m >>= maybe (return Nothing) (runMaybeT . g)


  fail _ =
    MaybeT $ return Nothing


instance MT.MonadTrans MaybeT where
  -- lift :: (Monad m) => m -> (MaybeT m) a
  lift m =
    MaybeT $ Just `M.liftM` m


instance (MT.MonadIO m) => MT.MonadIO (MaybeT m) where
  -- liftIO :: Monad m => IO a -> MaybeT m a
  liftIO m =
    MT.lift (MT.liftIO m)


instance (S.MonadState s m) => S.MonadState s (MaybeT m) where
  -- get :: MaybeT m a
  get =
    S.get

  -- put :: s -> MaybeT m ()
  put =
    S.put


instance (R.MonadReader r m) => R.MonadReader r (MaybeT m) where
  -- ask :: MonadReader m r => MaybeT m r
  ask =
    R.ask

  -- local :: MonadReader r m => (r -> r) -> MaybeT m a -> MaybeT m a
  local =
    R.local


instance (W.MonadWriter w m) => W.MonadWriter w (MaybeT m) where
  -- tell :: (W.MonadWriter w m) => w -> MaybeT m ()
  tell =
    W.tell

  -- listen :: (W.MonadWriter w m) => MaybeT m a -> MaybeT m (a, w)
  listen =
    W.listen

  -- pass :: (W.MonadWriter w m) => MaybeT m (a, w -> w) -> MaybeT m a
  pass =
    W.pass
