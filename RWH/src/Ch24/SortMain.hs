module Ch24.SortMain (main)

where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)
import Ch24.Sorting
import Control.Parallel.Strategies (rseq, evalList, using)


testFunction :: (Ord a) => [a] -> [a]
-- testFunction = sort
-- testFunction = seqSort
testFunction =
  parSort
-- testFunction = parSort2 2


randomInts :: Int -> StdGen -> [Int]
randomInts k g =
  let result =
        take k (randoms g)
  in force result `seq` result


main :: IO ()
main = do
  args <- getArgs
  let
    count | null args = 500000
          | otherwise = read (head args)
  input <- randomInts count `fmap` getStdGen
  putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
  start <- getCurrentTime
   -- explicitly trigger evaluation
  let sorted =
        testFunction input `using` evalList rseq
  -- length sorted (implicitly) triggers the evaluation of testFunction
  putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
