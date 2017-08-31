module Ch24.Sorting

where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (NFData, evalList, rdeepseq)


sort :: Ord a => [a] -> [a]
sort [] =
  []
sort (x:xs) =
  sort [y | y <- xs, y < x ] ++ x : sort [y | y <- xs, y >= x]


parSort :: Ord a => [a] -> [a]
parSort [] =
  []
parSort (x:xs) =
  force greater `par` (force lesser `pseq` (lesser ++ x:greater))
  where
    lesser =
      parSort [y | y <- xs, y <  x]

    greater =
      parSort [y | y <- xs, y >= x]


force :: [a] -> ()
force xs =
  go xs `pseq` ()
  where
    go [] =
      ()
    go (_:xs) =
      go xs


parSort' :: (Ord a, NFData a) => [a] -> [a]
parSort' [] =
  []
parSort' list@(x:xs)
  | length list <= 2 =
      sort list
  | otherwise =
      force greater `par` (force lesser `pseq` (lesser ++ x:greater))
  where
    force =
      evalList rdeepseq

    lesser =
      parSort' [y | y <- xs, y <  x]

    greater =
      parSort' [y | y <- xs, y >= x]
