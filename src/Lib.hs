module Lib
where


import Prelude hiding (sqrt, until)
import Data.Char (ord, chr)

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


zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 as bs cs ds =
  zipWith (\(a,b) (c,d) -> (a,b,c,d)) (zip as bs) (zip cs ds)


-- L - 1 + 3 => O(L) where L = lenght xs
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
