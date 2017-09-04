{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Ch24.BoundedChan ( BoundedChan
                        , newBoundedChan
                        , writeBoundedChan
                        , readBoundedChan
                        , main
                        )

where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.DeepSeq (NFData, force)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


type State =
  (MVar Int, Int)


newtype BoundedChan a =
  BoundedChan (MVar (State, Chan a))


newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan n = do
  ch <- newChan
  counter <- newMVar n
  mvar <- newMVar ((counter, n), ch)
  return (BoundedChan mvar)


writeBoundedChan :: BoundedChan a -> a -> IO ()
writeBoundedChan (BoundedChan mvar) msg = do
  (counter, ch) <- modifyMVar mvar $
    \bch@((counter, limit), ch) -> do
      mMVar <- tryTakeMVar counter
      case mMVar of
        Nothing ->
          return (bch, (counter, ch))
        Just x
          | x == 0 -> do
              empty <- newEmptyMVar
              -- it's ok here to discard the mvar since nobody can be waiting on it,
              -- the call to `modifyMVar mvar` assures this.
              return (((empty, limit), ch), (empty, ch))
          | otherwise -> do
              putMVar counter (x - 1)
              return (bch, (counter, ch))
  -- will block until counter is not empty
  withMVar counter $ const (writeChan ch msg)


readBoundedChan :: BoundedChan a -> IO a
readBoundedChan (BoundedChan mvar) = do
  ch <- modifyMVar mvar $
    \bch@((counter, limit), ch) -> do
      mMVar <- tryTakeMVar counter
      putMVar counter (maybe 1 (\x -> max limit (x + 1)) mMVar)
      return (bch, ch)
  -- will block until there's something to read from ch
  readChan ch


writeBoundedChan' :: NFData a => BoundedChan a -> a -> IO ()
writeBoundedChan' bch msg =
  writeBoundedChan bch msg'
  where
    !msg' =
      force msg
-- writeBoundedChan' bch (force -> !msg) =
--   writeBoundedChan bch msg


readBoundedChan' :: NFData a => BoundedChan a -> IO a
readBoundedChan' bch =
  return . force =<< readBoundedChan bch


{-
Wrote 'foo'
Wrote 'bar'
Read:foo
Read:bar
Wrote 'buzz'
*** Exception: thread blocked indefinitely in an MVar operation
-}
main :: IO ()
main = do
  ch <- newBoundedChan 2
  sem <- newMVar ()
  _ <- forkIO $ writeBoundedChan ch "foo" >> withMVar sem (\_ -> putStrLn "Wrote 'foo'")
  _ <- forkIO $ writeBoundedChan ch "bar" >> withMVar sem (\_ -> putStrLn "Wrote 'bar'")
  _ <- forkIO $ writeBoundedChan ch "buzz" >> withMVar sem (\_ -> putStrLn "Wrote 'buzz'")
  --threadDelay (10^3 * 500)
  xs1 <- readBoundedChan ch
  withMVar sem (\_ -> putStrLn $ "Read:" ++ xs1)
  xs2 <- readBoundedChan ch
  withMVar sem (\_ -> putStrLn $ "Read:" ++ xs2)
--  threadDelay (10^6)
  xs3 <- readBoundedChan ch
  putStrLn $ "Read:" ++ xs3
