module Lib
where


import Prelude hiding (sqrt, until)
import Data.Char (ord, chr)
import Data.List ((\\))


someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Ex. 2.1.7
-- sqrt :: (Ord n, Fractional n) => n -> [n]
sqrt n =
  newton f' n
  -- newton f n
  where
    f x =
      x * x

    f' x =
      x * x - n

-- newton :: (Ord n, Fractional n) => (n -> n) -> n -> [n]
newton f n =
  -- until satisfies improve n
  -- until satisfies' improve' n
  -- WHY until satisfies improve' n with f x = x * x DOESN'T WORK ?!
  until'' satisfies'' improve' n 0

  where
    -- until :: Fractional n => (n -> Bool) -> (n -> n) -> n -> [n] -> [n]
    until p g y =
      if p y then y else until p g (g y)

    satisfies x =
      abs (f x - n) < 0.0001

    -- improve :: Fractional n => n -> n -> n
    improve a =
      (a + n / a) / 2

    satisfies' x =
      abs (f x) < 0.0001 * x

    -- improve' :: Fractional n => (n -> n) -> n -> n
    improve' a =
      a - (f a / deriv f a)

      where
        deriv f a =
          (f (a + dx) - f a) / dx

        dx = 0.0001

    satisfies'' a a' =
      abs (a - a') < 0.0001 * abs a

    until'' p g y oldY =
      if p y oldY then y else until'' p g (g y) y


-- Ex. 2.2.2
sumsq x y z =
  sq (max x y) + sq (min (max y z) (max x z))

  where
    sq x =
      x * x

    max a b =
      if a > b then a else b

    min a b =
      if a < b then a else b


nextlet c =
  chr $ nextChar $ if isUpperCase c then 'A' else if isLowerCase c then 'a' else '\0'

  where
    nextChar start =
      ((ord c + 1) `mod` ord start) `mod` 26 + ord start

    isUpperCase c =
      ord c >= ord 'A' && ord c <= ord 'Z'

    isLowerCase c =
      ord c >= ord 'a' && ord c <= ord 'z'


digitval c =
  ord c - ord '0'


countNegative :: [Int] -> Int
countNegative xs =
  length [ x | x <- xs, x < 0 ]


intPairs :: Int -> [(Int, Int)]
intPairs n =
  [ (x, y) | x <- [1..n], y <- [1..n], x /= y ]


power :: Int -> Int -> Int
power x n =
  product [ x | _ <- [1..n] ]


divisors n =
  [ d | d <- [1..n], n `mod` d == 0 ]


prime n =
  mindivisor == [n]
  where
    mindivisor | divs == [] = []
               | otherwise = [minimum divs]
      where
        divs =
          [ x | x <- divisors n, x > 1 ]


gcd' =
  \a b -> maximum [ d | d <- divisors a, b `mod` d == 0 ]


-- Ex. 3.3.8
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 as bs cs ds =
  zipWith (\(a,b) (c,d) -> (a,b,c,d)) (zip as bs) (zip cs ds)


-- Ex. 3.3.9: L - 1 + 3 => O(L) where L = lenght xs
trips :: [a] -> [(a, a, a)]
trips xs | length xs < 3 =
  []
trips xs =
  (xs!!0, xs!!1, xs!!2) : trips (drop 1 xs)


trips' xs =
  tripsd xs []
  where
    tripsd xs' acc | length xs' < 3 =
                     reverse acc
                     -- acc
                   | otherwise =
                     tripsd (drop 1 xs') ((xs'!!0, xs'!!1, xs'!!2) : acc)
                     -- tripsd (drop 1 xs) (acc ++ [(xs!!0, xs!!1, xs!!2)])


-- Ex. 3.3.10
riffle :: [Int] -> [Int]
riffle xs =
  undefined


-- Ex. 3.3.1
type TheNumber = Int
type Guess = Int
score :: TheNumber -> Guess -> Int
score num guess =
  10 * countDigits * bulls numList guessList + cows
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
        toListn n acc =
          let
            (res, rem) = n `divMod` 10
          in
            toListn res (rem : acc)

    toNum =
      sum [ x * 10 ^ y | (x, y) <- zip numList [length numList - 1, length numList - 2..] ]

    countDigits =
      countDigitsd toNum 0
      where
        countDigitsd 0 acc =
          acc
        countDigitsd n acc =
          countDigitsd (n `div` 10) (acc + 1)

    bulls [] [] =
      0
    bulls (x:xs) (y:ys) =
      if x == y then 1 + bulls xs ys else bulls xs ys

    cows =
      (length numList) - (length $ numList \\ guessList) - bulls numList guessList


-- Ex. 3.4.1
filter' :: (a -> Bool) -> [a] -> [a]
filter' p =
  concat . map box
  where box x = if p x then [x] else []


-- Ex. 3.4.3
rewriteListComprehension1 =
  do
    let
      res1 = [ x | xs <- xss, x <- xs, odd x ]
      res2 = concat . map (filter odd) $ xss
    putStrLn $ "res1 = " ++ show res1
    putStrLn $ "res2 = " ++ show res2
    putStrLn $ "==? " ++ show(res1 == res2)
  where
    xss =
      [[1, 2, 3], [4, 5, 6]]

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
f = [ putStrLn (show (x, y)) | x <- [0..1000], x < 10, y <- [1..100] ]
g = [ putStrLn (show (x, y)) | x <- [0..1000], y <- [1..100], x < 10]
