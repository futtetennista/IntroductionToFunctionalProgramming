module Chapter6
where

import Chapter5 (perms)


kwic :: [String] -> String -> String
kwic inconsws =
  unlines . allUnwords . qsort . allPerms . filterIncons . allWords . fullstopLines
  where
    allUnwords =
      map unwords

    allPerms =
      foldr ((++) . map perms) []
      --concat . map perms

    allWords =
      map words

    filterIncons =
      filter (not . inconsequential . head)

    inconsequential xs =
      or (map (==xs) inconsws)
      -- foldr ((||) . (xs==)) False inconsws

    fullstopLines =
      map (++".") . lines

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
      (min x x', max y y')

-- ex.: findSmallest (>23) 10
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


type Board =
  [Int]

queens :: Int -> [Board]
queens 0 =
  [[]]
queens n =
  [pos ++ [row] | pos <- queens (n - 1), row <- [1..8], safe pos row]


sneeuq :: Int -> [Board]
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


-- Ex. 6.5.1
-- `sneeuq` doesn't support this optimisation because the row is created before the positions so it's not possible to filter it out
queens' :: Int -> [Board]
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


putSolutions :: [Board] -> IO ()
putSolutions =
  mapM_ putBoard


putBoard :: Board -> IO ()
putBoard =
  putStrLn . showBoard


showBoard :: Board -> String
showBoard pos =
  unlines (header ++ content (zip ([1..8] :: [Int]) pos))
  where
    header =
      ["  1 2 3 4 5 6 7 8"]

    content =
      foldr ((:) . indexedRow) []

    indexedRow (idx, x) =
      show idx ++ " " ++ row x

    row p =
      foldr (\x acc -> (if x == p then "♛ " else ". ") ++ acc) [] [1..8]


-- Ex. 6.5.3
data MirroringOptions
  = Vertical
  | Horizontal
  | DiagonalTLBR -- from top-left to bottom-right
  | DiagonalTRBL -- from top-right to bottom-left
  | Rotation -- 90 degrees clockwise
  deriving Show


mirror :: MirroringOptions -> Board -> Board
mirror Vertical =
  unindexBoard . map (\(c, r) -> (c, 9 - r)) . indexBoard
mirror Horizontal =
  reverse
mirror DiagonalTLBR =
  mirrorDiag [1..8]
  -- unindexBoard (qsortBy fst indexedRows)
  -- where
  --   indexedRows =
  --     map (\(c, r) -> (r, c)) (zip [1..8] b)
mirror DiagonalTRBL =
  mirrorDiag [8,7..1]
mirror Rotation =
  undefined


type IndexedBoard =
  [(Int, Int)]


indexBoard :: Board -> IndexedBoard
indexBoard =
  zip [1..8]


unindexBoard :: IndexedBoard -> Board
unindexBoard =
  map snd


mirrorDiag :: Board -> Board -> Board
mirrorDiag diag board =
  unindexBoard . qsortBy fst $ mirrorIBoard
  where
    mirrorIBoard =
      map mirrorPos (zip [1..8] board)

    mirrorPos (c,r) =
      (mirrorLine r, mirrorLine c)

    mirrorLine x =
      fst . head . filter ((==x) . snd) $ idiag

    idiag =
      zip [1..8] diag

    qsortBy :: Ord b => (a -> b) -> [a] -> [a]
    qsortBy _ [] =
      []
    qsortBy f (x:xs) =
      qsortBy f [y | y <- xs, f y <= f x] ++ [x] ++ qsortBy f [y | y <- xs, f y > f x]
