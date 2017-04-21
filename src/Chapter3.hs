module Chapter3
where


import Data.List ((\\))


countNegative :: [Int] -> Int
countNegative xs =
  length [ x | x <- xs, x < 0 ]


intPairs :: Int -> [(Int, Int)]
intPairs n =
  [ (x, y) | x <- [1..n], y <- [1..n], x /= y ]


power :: Int -> Int -> Int
power x n =
  product [ x | _ <- [1..n] ]


divisors :: Int -> [Int]
divisors n =
  [ d | d <- [1..n `div` 2], n `mod` d == 0 ] ++ [n]


prime :: Int -> Bool
prime n =
  mindivisor == [n]
  where
    mindivisor
      | divs == [] =
          []
      | otherwise =
          [minimum divs]

    divs =
      [ x | x <- divisors n, x > 1 ]


gcd' :: Int -> Int -> Int
gcd' =
  \a b -> maximum [ d | d <- divisors a, b `mod` d == 0 ]


-- Ex. 3.3.8
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 as bs cs ds =
  zipWith (\(a,b) (c,d) -> (a,b,c,d)) (zip as bs) (zip cs ds)


-- Ex. 3.3.9: L - 1 + 3 => O(L) where L = length xs
trips :: [a] -> [(a, a, a)]
trips xs | length xs < 3 =
  []
trips xs =
  (xs!!0, xs!!1, xs!!2) : trips (drop 1 xs)

checkTrips :: Bool
checkTrips =
  trips [1..5] == [(1, 2, 3), (2, 3, 4), (3, 4, 5)]


trips' :: [a] -> [(a, a, a)]
trips' xs =
  tripsd xs []
  where
    tripsd xs' acc | length xs' < 3 =
                     reverse acc
                     -- acc
                   | otherwise =
                     tripsd (drop 1 xs') ((xs'!!0, xs'!!1, xs'!!2) : acc)
                     -- tripsd (drop 1 xs) (acc ++ [(xs!!0, xs!!1, xs!!2)])

checkTrips' :: Bool
checkTrips' =
  trips' [1..5] == [(1, 2, 3), (2, 3, 4), (3, 4, 5)]


-- Ex. 3.3.10
riffle :: [Int] -> [Int]
riffle xs =
  concat [[a, b] | (a, b) <- zippedOddsEvens]
  where
    zippedOddsEvens =
      zip (xs \\ evens) evens

    evens =
      [x | x <- xs, x `mod` 2 == 0]


checkRiffle :: Bool
checkRiffle =
  riffle ([1, 3..7] ++ [2, 4..8]) == [1..8]


-- Ex. 3.3.1
type TheNumber = Int
type Guess = Int

score :: TheNumber -> Guess -> String
score num guess =
  show (10 * countDigits num 0 * bulls numList guessList) ++ show cows
  where
    numList =
      toList num

    guessList =
      toList guess
    -- n =
    --  floor . logBase 10 $ (fromIntegral toNum)
    toList n =
      toListn n []
      where
        toListn 0 acc =
          acc
        toListn x acc =
          let
            (res, rest) = x `divMod` 10
          in
            toListn res (rest : acc)

    toNum =
      sum [ x * 10 ^ y | (x, y) <- zip numList [length numList - 1, length numList - 2..] ]

    countDigits 0 acc =
      acc
    countDigits n acc =
      countDigits (n `div` 10) (acc + 1)

    bulls [] [] =
      0
    bulls (x:xs) (y:ys) =
      if x == y then 1 + bulls xs ys else bulls xs ys

    cows =
      (length numList) - (length $ numList \\ guessList) - bulls numList guessList


checkScore :: Bool
checkScore =
  [ score 2113 1234
  , score 2113 1111
  , score 2113 1212
  ] == [ "03", "20", "12" ]


-- Ex. 3.4.1
filter' :: (a -> Bool) -> [a] -> [a]
filter' p =
  concat . map box
  where
    box x =
      if p x then [x] else []


-- Ex. 3.4.3
rewriteListComprehension1 =
  do
    let
      res1 = [ x | xs <- xss, x <- xs, odd x ]
      res2 = concat . map (filter odd) $ xss
    putStrLn $ "res1 = " ++ show res1
    putStrLn $ "res2 = " ++ show res2
    putStrLn $ "==? " ++ show (res1 == res2)
  where
    xss =
      [[1..5], [10..15]]

    odd x =
      x `mod` 2 /= 0


rewriteListComprehension2 =
  do
    let
      res1 = [ (x, y) | x <- xs, odd x,  y <- ys ]
      res2 =  concat . map mkPairs $ filter odd xs
    putStrLn $ "res1 = " ++ show res1
    putStrLn $ "res2 = " ++ show res2
    putStrLn $ "==? " ++ show(res1 == res2)
  where
    xs =
      [1, 2, 3, 4, 5, 6]

    ys =
      [10, 11, 12]

    mkPairs ::a -> [(a, Int)]
    mkPairs x =
      map ((,) x) ys

    odd x =
      x `mod` 2 /= 0

-- Ex. 3.4.4
-- f more performant since filtering is applied before ys are generated
earlyFilterListComprehension =
  [ putStrLn (show (x, y)) | x <- [0..1000], x < 10, y <- [1..100] ]

lateFilterListComprehension =
  [ putStrLn (show (x, y)) | x <- [0..1000], y <- [1..100], x < 10 ]


-- Ex. 3.5.1
all' :: (a -> Bool) -> [a] -> Bool
all' p xs =
  foldl (\acc x -> acc && p x) True xs


-- Ex. 3.5.2
foldlTrue x xs =
  foldl (-) x xs == x - sum xs


foldrFalse x xs =
  foldr (-) x xs /= x - sum xs


-- Ex. 3.5.3
isomorphicFoldlFuncs :: Eq b => (b -> a -> b) -> b -> [a] -> [a] -> Bool
isomorphicFoldlFuncs f zero xs ys =
  foldl f zero (xs ++ ys) == foldl f (foldl f zero xs) ys -- the equality holds since foldl starts from the left-most element


-- using reverse (xs ++ ys) == reverse ys ++ reverse xs and the 3rd duality law
isomorphicFoldrFuncs :: Eq b => (a -> b -> b) -> b -> [a] -> [a] -> Bool
isomorphicFoldrFuncs f zero xs ys =
  foldr f zero (xs ++ ys) == foldl (flip f) (foldl (flip f) zero $ reverse ys) (reverse xs)
  -- foldr f zero (xs ++ ys) == foldr f (foldr f zero ys) xs


-- Ex. 3.5.4
insert :: Ord a => a -> [a] -> [a]
insert x xs =
  takeWhile (<= x) xs  ++ [x] ++ dropWhile (<= x) xs


-- Time complexity: takeWhile will iterate through n/2 on average, same for dropWhile. This will happen n times so O(n^2).
isort :: Ord a => [a] -> [a]
isort =
  foldr insert []


-- Ex. 3.5.5
remdups :: Ord a => [a] -> [a]
remdups =
  reverse . foldl (\acc x -> if elem x acc then acc else x : acc) []

-- fastRemdups :: Ord a => [a] -> [a]
-- fastRemdups =
--   toList . toSet
--   where
--     toList :: Set a -> [a]
--     toList Empty =
--       []
--     toList (Node k l r) =
--       toList l ++ [k] ++ toList r

--     toSet :: Ord a => [a] -> Set a
--     toSet =
--      foldl add Empty

-- data Set a
--   = Node { key :: a
--          , left :: Set a
--          , right :: Set a
--          }
--   | Empty

-- add :: Ord a => Set a -> a -> Set a
-- add Empty x =
--   Node { key = x, left = Empty, right = Empty }

-- add node@(Node k _ _) x
--   | k == x = node
--   | otherwise =
--     addLevelOrder node x

-- addLevelOrder set x =
--   undefined


-- Ex. 3.5.7
ssum :: Ord a => [a] -> [a]
ssum [] =
  []

ssum xs =
  reverse $ foldl localMax ([head xs]) xs
  where
    localMax acc@(y:_) x =
      if x > y then x : acc else acc

    localMax [] _ =
      fail "Accumulator must be a singleton list"

testSsum :: Bool
testSsum =
  (ssum [3, 1, 3, 4, 9, 2, 10, 7] :: [Int]) == [3, 4, 9, 10]

-- Ex. 3.5.8
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs =
  foldr f (last xs) (init xs)


foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f xs =
  foldl f (head xs) (tail xs)


scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' _ [] =
  []
scanl1' f (x:xs) =
  scanl f x xs

scanr1' :: (a -> a -> a) -> [a] -> [a]
scanr1' _ [] =
  []
scanr1' f xs =
  scanr f (last xs) (init xs)


-- Ex. 3.5.9
type Precision =
  Int

computeE :: Precision -> Double
computeE x =
  foldl (\acc y -> (1 / fact y 1) + acc) 0 $ take x [0..]
  where
    fact n acc
      | n <= 0 =
        acc
      | otherwise =
        fact (n - 1) (n * acc)


-- Ex. 3.6.3
rev2 :: [a] -> [a]
rev2 (x:y:[]) =
  (y:x:[])

rev2 xs =
  xs


-- Ex. 3.6.4
insert' :: Ord a => a -> [a] -> [a]
insert' x =
  foldr swap [x]
  where
    swap :: Ord a => a -> [a] -> [a]
    swap y (z:zs) =
      if y > z then z:y:zs else y:z:zs
    swap _ [] =
      fail "Accumulator must be a non-empty list"


-- Time complexity: 1 + 2 + … + n swaps =~ n^2/2 = O(n^2)
isort' :: Ord a => [a] -> [a]
isort' =
  foldr insert' []
