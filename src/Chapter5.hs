module Chapter5
where

import Chapter4 (rjustify)

binom :: Int -> Int -> Int
binom n k
  | n < 0 =
    0
  | k == 0 =
    1
  | otherwise =
    binom (n - 1) k + binom (n - 1) (k - 1)


pascal :: Int -> [[Int]]
pascal x =
  [map toBinom xs | xs <- table]
  where
    toBinom (n, k) =
      binom n k

    elemCount =
      x + 1

    table =
      [take elemCount (drop n elems) | n <- [0, elemCount..(length elems - elemCount)]]

    elems =
      [(a, b) | a <- [0..x], b <- [0..x]]


printPascal :: Int -> IO()
printPascal x =
  mapM_ putStr (printableTable (pascal x))
  where
    printableTable :: [[Int]] -> [String]
    printableTable xss =
      map (printableRow (rjustify (longestDigit xss + 1))) xss

    printableRow rjustified xs =
      foldr ((++) . rjustified  . show) "\n" xs

    longestDigit :: [[Int]] -> Int
    longestDigit =
      -- maximum . concat . map (map digits)
      maximum . map localLongestDigit

    localLongestDigit :: [Int] -> Int
    localLongestDigit =
      maximum . map digits

    digits :: Integral a => Int -> a
    digits n =
      floor $ logBase 10.0 (fromIntegral n) + 1
