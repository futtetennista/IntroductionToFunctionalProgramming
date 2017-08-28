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
  ((counter, _), ch) <- readMVar mvar
  n <- modifyMVar counter $
    \x -> do writeChan ch msg ; return (x - 1, x - 1)
  modifyMVar_ mvar $ \(st, ch) ->
   if n == 0
   then do x <- newEmptyMVar ; return (first (const x) st, ch)
   else return (st, ch)


readBoundedChan :: BoundedChan a -> IO a
readBoundedChan (BoundedChan mvar) = do
  ((counter, n), ch) <- readMVar mvar
  flg <- isEmptyMVar counter
  if flg
    then putMVar counter 1
    else modifyMVar_ counter (\x -> return (if x < n then x + 1 else x))
  readChan ch


main :: IO ()
main = do
  ch <- newBoundedChan 2 :: IO (BoundedChan [Char])
  sem <- newMVar ()
  forkIO $ withMVar sem (\_ -> writeBoundedChan ch "foo" >> putStrLn "Wrote 'foo'")
  forkIO $ withMVar sem (\_ -> writeBoundedChan ch "bar" >> putStrLn "Wrote 'bar'")
  forkIO $ withMVar sem (\_ -> writeBoundedChan ch "buzz" >> putStrLn "Wrote 'buzz'")
  threadDelay (1000*1000)
  putStrLn . ("Read:"++) =<< readBoundedChan ch
  putStrLn . ("Read:"++) =<< readBoundedChan ch
  threadDelay (10^6)
  putStrLn . ("Read:"++) =<< readBoundedChan ch
  return ()
