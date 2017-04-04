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


binomSum :: Int -> Int
binomSum n =
  sum [binom n k | k <- [0..n]]


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


(!) :: [a] -> Int -> a
[]!_ =
  error "empty list"
(x:xs)!i
  | i == 0 =
    x
  | otherwise =
    xs!(i - 1)


takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ [] =
  []
takewhile p (x:xs)
  | p x =
    x : takewhile p xs
  | otherwise =
    []


dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile _ [] =
  []
dropwhile p ys@(x:xs)
  | p x =
    dropwhile p xs
  | otherwise =
    ys


inits :: [a] -> [[a]]
inits [] =
  [[]]
inits (x:xs) =
  [[]] ++ map (x:) (inits xs)


subs :: [a] -> [[a]]
subs [] =
  [[]]
subs (x:xs) =
  subs' ++ map (x:) subs'
  where
    subs' =
      subs xs


intersperse :: a -> [a] -> [[a]]
intersperse x [] =
  [[x]]
intersperse x (y:ys) =
  [x:y:ys] ++ map (y:) (intersperse x ys)


perms :: [a] -> [[a]]
perms [] =
  [[]]
perms (x:xs) =
  concat . map (intersperse x) $ perms xs


perms' :: [a] -> [[a]]
perms' [] =
  [[]]
perms' (x:xs) =
  [zs | ys <- perms xs, zs <- intersperse x ys]


-- 5.6.1
-- length segs = sum [1..length xs] + 1
segs :: [a] -> [[a]]
segs [] =
  [[]]
segs (x:xs) =
  segs xs ++ map (x:) (inits xs)

segs' :: [a] -> [[a]]
segs' xs =
  segstr xs [[]]
  -- segstr xs []
  where
    segstr [] acc =
      acc
      -- [[]] ++ acc
    segstr (y:ys) acc =
      segstr ys (map (y:) (inits ys) ++ acc)


-- 5.6.2
-- length (choose k xs) == binom (length xs) k
choose :: Int -> [a] -> [[a]]
choose _ [] =
  [[]]
choose 0 _ =
  [[]]
choose k xs@(y:ys)
  | length xs == k =
    [xs]
  | otherwise =
    choose k ys ++ map (y:) (choose (k - 1) ys)
