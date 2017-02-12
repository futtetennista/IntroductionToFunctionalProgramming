module Chapter2
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
