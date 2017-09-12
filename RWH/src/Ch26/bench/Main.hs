{-# LANGUAGE TypeSynonymInstances #-}
module Main
where

import qualified BloomFilter.BloomFilter as BloomFilter
import Control.Monad (forM_, mapM_)
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.DeepSeq (rnf)
import Prelude hiding (words)
import Control.Parallel.Strategies (NFData)
import Weigh


main :: IO ()
main = do
  profiling
  --weigh


weigh :: IO ()
weigh = do
  ws <- fmap BS.lines $ BS.readFile "/usr/share/dict/words"
  mainWith $ do
    func "sizing" (BloomFilter.suggestSizing (fromIntegral (length ws))) 0.01
    func "lazy creation" (BloomFilter.mkFromList 0.01) ws
    -- validateNFValue "strict creation" (BloomFilter.mkFromList' 0.01 ws) (BloomFilter.mkFromList' 0.01) ws
    let
      ebfilt =
        BloomFilter.mkFromList 0.01 ws
    case ebfilt of
      Left err ->
        errorWithoutStackTrace err

      Right bfilt ->
        action "query every element" $
          mapM_ print $ filter (not . (`BloomFilter.elem` bfilt)) ws


profiling :: IO ()
profiling = do
  args <- getArgs
  let files
        | null args =
            ["/usr/share/dict/words"]
        | otherwise =
            args
  forM_ files $ \file -> do
    words <- timed "read words" $
      BS.lines `fmap` BS.readFile file
    let
      len =
        length words
      errRate =
        0.01
    putStrLn $ show len ++ " words"
    putStrLn $ "suggested sizings: "
      ++ show (BloomFilter.suggestSizing (fromIntegral len) errRate)
    filt <- timed "construct filter" $
      case BloomFilter.mkFromList errRate words of
        Left errmsg -> do
          putStrLn $ "Error: " ++ errmsg
          exitFailure

        Right filt ->
          return filt

    timed "query every element" $
      mapM_ print $ filter (not . (`BloomFilter.elem` filt)) words

timed :: (NFData a) => String -> IO a -> IO a
timed desc action = do
  start <- getCurrentTime
  ret <- action
  end <- rnf ret `seq` getCurrentTime
  putStrLn $ show (diffUTCTime end start) ++ " to " ++ desc
  return ret
