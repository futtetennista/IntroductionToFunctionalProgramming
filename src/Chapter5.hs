module Chapter5
where

import Chapter4 (rjustify)


-- 5.1.3
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
  [map (uncurry binom) xs | xs <- xss]
  where
    elemCount =
      x + 1

    xss =
      [take elemCount (drop n elems) | n <- [0, elemCount..(length elems - elemCount)]]

    elems =
      [(a, b) | a <- [0..x], b <- [0..x]]


printPascal :: Int -> IO()
printPascal x =
  mapM_ putStr table
  where
    pascalTriangle =
      pascal x

    table =
      header ++ ["\n"] ++ divisor ++ ["\n"] ++ content

    justification =
      1 + (longestDigit pascalTriangle)

    divisor =
      take (2 + (length pascalTriangle + 1) * justification) (repeat "-")

    content =
      map row (zip pascalTriangle [0..length pascalTriangle])

    row (r, idx) =
      rowIndex idx ++ rowContent (rjustify justification) r

    rowIndex idx =
      (rjustify justification (show idx)) ++ " |"

    header =
      ["  " ++ rjustify justification "|"] ++ map ((rjustify justification) . show) [0..length pascalTriangle - 1]

    rowContent rjustified xs =
      foldr ((++) . rjustified . showNonZero) "\n" xs

    showNonZero y =
      if y > 0 then show y else " "

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
