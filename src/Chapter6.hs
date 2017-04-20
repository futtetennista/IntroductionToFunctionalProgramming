module Chapter6
where

import Chapter4 (copy)
import Chapter5 (perms)


kwic :: [String] -> String -> String
kwic iws =
  unlines . map unwords . qsort . concat . map perms . filterTitles . map words . fullstopLines
  where
    filterTitles xss =
      filter (not . inconsequential . head) xss

    inconsequential xs =
      foldr ((||) . (xs==)) False iws

    fullstopLines =
      map (++ ".") . lines

    qsort [] =
      []
    qsort (x:xs) =
      qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

kwicTestInput :: String
kwicTestInput =
  "Casablanca\nCitizen Kane\nCharlie Bubbles\nDuck Soup\nThe Great Gatsby"


minmax :: Ord a => [a] -> (a, a)
minmax [x] =
  (x, x)
minmax xs =
  merge (minmax ls) (minmax rs)
  where
    ls =
      take mid xs

    rs =
      drop mid xs

    mid =
      length xs `div` 2

    merge (x, y) (x', y') =
      (if x < x' then x else x', if y > y' then y else y')

-- findSmallest (>23) 10
type MonotonicPred =
  (Int -> Bool)

findSmallest :: MonotonicPred -> Int -> Int
findSmallest p start =
  find start start
  where
    find a b
      | a == b && p a =
        a
      | a >= b && not (p a) =
        find (2 * a) a
      | a > b && p a =
        find (b + 1) a
      | a < b && p mid =
        find a mid
      | a < b && not (p mid) =
        find (mid + 1) b
      where
        mid =
          (a + b) `div` 2


queens :: Int -> [[Int]]
queens 0 =
  [[]]
queens n =
  [pos ++ [row] | pos <- queens (n - 1), row <- [1..8], safe pos row]


sneeuq :: Int -> [[Int]]
sneeuq 0 =
  [[]]
sneeuq n =
  [pos ++ [row] | row <- [1..8], pos <- ps, safe pos row]
  where
    ps =
      sneeuq (n - 1)


safe :: [Int] -> Int -> Bool
safe pos queryRow =
  and [not . check (c, r) $ (nextCol, queryRow) | (c, r) <- zip [1..length pos] pos]
  where
    nextCol =
      length pos + 1

    check (col, row) (col', row') =
      row == row' || col + row == col' + row' || col - row == col' - row'


-- 6.5.1
-- `sneeuq` doesn't support this optimisation because the row is created before the positions so it's not possible to filter it out
queens' :: Int -> [[Int]]
queens' 0 =
  [[]]
queens' n =
  [pos ++ [row] | pos <- queens' (n - 1), row <- [1..8], free row pos, safe' pos row]
  where
    free row pos =
      [] == filter (==row) pos

    safe' pos queryRow =
      and [not . check (c, r) $ (length pos + 1, queryRow) | (c, r) <- zip [1..length pos] pos, r /= queryRow]

    check (col, row) (col', row') =
      col + row == col' + row' || col - row == col' - row'


putSolutions :: [[Int]] -> IO ()
putSolutions =
  mapM_ putBoard


putBoard :: [Int] -> IO ()
putBoard =
  putStrLn . board


board :: [Int] -> String
board pos =
  unlines (header ++ content (zip ([1..8] :: [Int]) pos))
  where
    header =
      ["  1 2 3 4 5 6 7 8"]

    content =
      foldr ((:) . indexedRow) []

    indexedRow (idx, x) =
      show idx ++ " " ++ row x

    row p =
      foldr (\x acc -> if x == p then "♕ " else "  " ++ acc) [] [1..8]
