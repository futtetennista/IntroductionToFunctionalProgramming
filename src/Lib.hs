module Lib
    ( someFunc
    , sqrt
    )
where


import Prelude hiding (sqrt, until)


someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Ex. 2.1.17
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
