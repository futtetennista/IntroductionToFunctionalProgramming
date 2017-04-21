module Chapter7
where

import Prelude hiding (iterate)


iterate :: (a -> a) -> a -> [a]
iterate f x =
  x : iterate f (f x)


-- Ex. 7.2.3
-- TODO: make it total
showint :: Int -> String
showint i =
  signed . map char . digits $ abs i
  where
    signed xs
      | i < 0 =
        '-' : xs
      | otherwise =
        xs

    char d =
      head (drop d "0123456789")

    digits =
      reverse . map (`mod` 10) . takeWhile (/=0) . iterate (`div` 10)


-- Ex. 7.2.4
-- TODO: make it total
getint :: [Char] -> Int
getint xs =
  signed . undigits . map digit $ absstr xs
  where
    signed n =
      if (head xs == '-') then 0 - n else n

    absstr =
      dropWhile (=='-')

    digit =
      fst . head . findDigit

    findDigit x =
      dropWhile ((/=x) . snd) $ numChars

    numChars =
      zip [0..9] "0123456789"

    undigits ys =
      sum [n * 10^p | (n, p) <- pows ys]

    pows ys =
      zip ys (reverse [0..length ys - 1])
