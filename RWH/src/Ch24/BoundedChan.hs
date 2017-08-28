module Ch24.BoundedChan

where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Arrow (first)
import Control.Concurrent
import Control.Exception


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
      case mMVar of
        Nothing ->
          putMVar counter 1
        Just x ->
          putMVar counter $ max limit (x + 1)
      return (bch, ch)
  -- will block until there's something to read from ch
  readChan ch


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
  forkIO $ writeBoundedChan ch "foo" >> withMVar sem (\_ -> putStrLn "Wrote 'foo'")
  forkIO $ writeBoundedChan ch "bar" >> withMVar sem (\_ -> putStrLn "Wrote 'bar'")
  forkIO $ writeBoundedChan ch "buzz" >> withMVar sem (\_ -> putStrLn "Wrote 'buzz'")
  --threadDelay (10^3 * 500)
  xs <- readBoundedChan ch
  withMVar sem (\_ -> putStrLn $ "Read:" ++ xs)
  xs <- readBoundedChan ch
  withMVar sem (\_ -> putStrLn $ "Read:" ++ xs)
--  threadDelay (10^6)
  xs <- readBoundedChan ch
  putStrLn $ "Read:" ++ xs
