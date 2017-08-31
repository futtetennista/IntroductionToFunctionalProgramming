{-# LANGUAGE BangPatterns #-}
module Ch24.MVar ( MVar
                 , newEmptyMVar
                 , newMVar
                 , takeMVar
                 , putMVar
                 , readMVar
                 , swapMVar
                 , tryTakeMVar
                 , tryPutMVar
                 , isEmptyMVar
                 , withMVar
                 , withMVarMasked
                 , modifyMVar_
                 , modifyMVar
                 , modifyMVarMasked_
                 , modifyMVarMasked
                 , tryReadMVar
                 , mkWeakMVar
                 -- , addMVarFinalizer
                 )
 where


import qualified Control.Concurrent.MVar as Lazy
import Control.DeepSeq (NFData, force, deepseq)
import GHC.Weak (Weak, mkWeak)


newtype MVar a =
  MVar { unwrap :: Lazy.MVar a }


newEmptyMVar :: NFData a => IO (MVar a)
newEmptyMVar = do
  mvar <- Lazy.newEmptyMVar
  _ <- Lazy.withMVarMasked mvar $ return . force
  return (MVar mvar)


newMVar :: NFData a => a -> IO (MVar a)
newMVar x = do
  mvar <- Lazy.newMVar x
  _ <- Lazy.withMVarMasked mvar (return . force)
  return (MVar mvar)


takeMVar :: MVar a -> IO a
takeMVar =
  Lazy.takeMVar . unwrap


putMVar :: NFData a => MVar a -> a -> IO ()
putMVar (MVar wrapped) !x =
  x `deepseq` Lazy.putMVar wrapped x


readMVar :: MVar a -> IO a
readMVar =
  Lazy.readMVar . unwrap


swapMVar :: MVar a -> a -> IO a
swapMVar =
  Lazy.swapMVar . unwrap


tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar =
  Lazy.tryTakeMVar . unwrap


tryPutMVar :: NFData a => MVar a -> a -> IO Bool
tryPutMVar (MVar wrapped) !x =
  x `deepseq` Lazy.tryPutMVar (wrapped) x


isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar =
  Lazy.isEmptyMVar . unwrap


withMVar :: NFData b => MVar a -> (a -> IO b) -> IO b
withMVar (MVar wrapped) f =
  Lazy.withMVar wrapped f >>= return . force


withMVarMasked :: NFData b => MVar a -> (a -> IO b) -> IO b
withMVarMasked (MVar wrapped) f =
  Lazy.withMVar wrapped f >>= return . force


modifyMVar_ :: NFData a => MVar a -> (a -> IO a) -> IO ()
modifyMVar_ (MVar wrapped) f =
  Lazy.modifyMVar_ wrapped (\x -> f x >>= return . force)


modifyMVar :: (NFData a, NFData b) => MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (MVar wrapped) f =
  Lazy.modifyMVar wrapped (\x -> f x >>= return . force)


modifyMVarMasked_ :: NFData a => MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ (MVar wrapped) f =
  Lazy.modifyMVarMasked_ wrapped (\x -> f x >>= return . force)


modifyMVarMasked :: (NFData a, NFData b) => MVar a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked (MVar wrapped) f =
  Lazy.modifyMVarMasked wrapped (\x -> f x >>= return . force)


tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar =
  Lazy.tryReadMVar . unwrap


-- not too sure about this...but it type-checks
mkWeakMVar :: NFData a => MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar mvar@(MVar wrapped) io =
  mkWeak wrapped mvar (Just io)
