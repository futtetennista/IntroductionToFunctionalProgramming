#!/usr/bin/env stack
-- stack script --resolver lts-8.22

{-# LANGUAGE BangPatterns #-}
module AsyncPlayground

where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async hiding (wait, waitCatch)
import Control.Exception (SomeException, handle, displayException, throwIO)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Void (absurd)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, writeTChan, readTChan)
import Control.Monad (forever)


-- Taken from https://haskell-lang.org/library/async
action1 :: IO Int
action1 =
  error "action1 errored"


action2 :: IO String
action2 =
  handle onErr $ do
    threadDelay 500000
    return "action2 completed"
  where
    onErr e = do
      putStrLn $ "action2 was killed by: " ++ displayException e
      throwIO (e :: SomeException)


action2' :: IO T.Text
action2' =
  handle onErr $ do
    threadDelay 500000
    return "action2 completed"
  where
    onErr e = do
      TIO.putStrLn . T.pack $ "action2 was killed by: " ++ displayException e
      throwIO (e :: SomeException)


main1 :: IO ()
main1 = do
  res <- concurrently action1 action2'
  print res


-- | Print successive numbers to stdout. Notice how it returns @a@ instead of
-- @()@. This lets the type system know that, under normal circumstances, this
-- function will never exit.
counter :: IO a
counter =
  let loop i = do
        putStrLn $ "counter: " ++ show i
        threadDelay 1000000
        loop $! (i + 1 :: Int)
  in loop 1


-- | This function will continue to run counter with whatever action you've
-- provided, and stop running counter once that action exits. If by some chance
-- counter throws an exception, it will take down your thread as well.
withCounter :: IO a -> IO a
-- withCounter inner = do
--   res <- race counter inner
--   case res of
--     Left x ->
--       -- cannot happen since `inner` always exits before `counter`
--       assert False x

--     Right x ->
--       return x
withCounter =
  fmap (either absurd id) . race counter


main2 :: IO ()
main2 = do
  putStrLn "Before withCounter"
  threadDelay 2000000
  withCounter $ do
    threadDelay 2000000
    putStrLn "Inside withCounter"
    threadDelay 2000000
  threadDelay 2000000
  putStrLn "After withCounter"
  threadDelay 2000000
  putStrLn "Exiting!"


wait :: Async a -> IO a
wait a =
  either throwIO return =<< waitCatch a


{-
Efficiently implement `waitCatch` in terms of `poll`.
1. there's a `threadWaitRead` but it requires a `Fd` that I don't know how to
   get from a `ThreadId`, which is the only thing known about the thread
2. leveraging MVars or Chan not sure how though

It turns out it's not possible! STM to the rescue.
-}
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = do
  undefined


data Work =
  Work !T.Text


jobQueue :: TChan Work -> IO a
jobQueue chan =
  forever $ do
    Work t <- atomically $ readTChan chan
    print t


main4 :: IO ()
main4 = do
  chan <- newTChanIO
  a <- async $ jobQueue chan
  -- without linking, the main thread runs indefinitely without doing anything
  link a
  forever $ do
    atomically $ do
      writeTChan chan $ Work "Hello"
      -- not evaluated here: written lazily to the channel
      -- and the Work construction is lazy for `data Work = Work T.Text`
      -- writeTChan chan $ Work undefined
      -- evaluated here for `data Work = Work T.Text`
      -- writeTChan chan $! Work $! undefined
      -- evaluated here for `data Work = Work !T.Text`
      -- tl;dr: writeTChan is lazy in its arguments so evaluation must be forced!
      writeTChan chan $! Work undefined
      writeTChan chan $ Work "World"
    threadDelay 1000000


main4' :: IO ()
main4' = do
  chan <- newTChanIO
  _ <- race (jobQueue chan) (forever $ do
                                atomically $ do
                                  writeTChan chan $ Work "Hello"
                                  writeTChan chan $ Work undefined
                                  writeTChan chan $ Work "World"
                                threadDelay 1000000)
  return ()
