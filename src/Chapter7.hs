module Chapter7
where

import Prelude hiding (iterate)


-- Ex. 7.2.1
powerlist :: [[Integer]]
powerlist =
  [[x ^ y | y <- ([0..]::[Int])] | x <- [1..]]


powerlistOf :: Int -> [Integer]
powerlistOf n =
  powerlist !! (n - 1)


iterate :: (a -> a) -> a -> [a]
iterate f x =
  x : iterate f (f x)


-- Ex. 7.2.2
range :: Int -> Int -> Int -> [Int]
range a b c
  | not monotonicRange =
    []
  | otherwise =
    takeWhile within (iterate ((b - a) +) a)
  where
    monotonicRange =
      (b <= a && c <= a) || (b >= a && c >= a)

    within x
      | c > a =
        x <= c
      | c < a =
        x >= c
      | otherwise =
        x == c


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


primes :: [Integer]
primes =
  rsieve [2..]
  where
    rsieve (p:xs) =
      p : rsieve [x | x <- xs, x `mod` p /= 0]


-- Ex. 7.3.1
firstPrimeGreaterThan1000 :: Integer
firstPrimeGreaterThan1000 =
  head . dropWhile (<1000) $ primes


hamming :: [Integer]
hamming =
  1 : merge (map (2*) hamming)
            (merge (map (3*) hamming)
                   (map (5*) hamming))


merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
  | x == y =
      x : merge xs ys
  | x < y =
      x : merge xs (y:ys)
  | y < x =
      y : merge (x:xs) ys


-- ƛ: take 500 (hamming' 2 3 5)
-- (11.84 secs, 4,892,275,184 bytes)
hamming' :: Integer -> Integer -> Integer -> [Integer]
hamming' a b c =
  1 : merge (map (a*) (hamming' a b c))
            (merge (map (b*) (hamming' a b c))
                   (map (c*) (hamming' a b c)))

-- Ex. 7.6.4
-- ƛ: take 500 (fasthamming' 2 3 5)
-- (0.01 secs, 3,339,976 bytes)
fasthamming' :: Integer -> Integer -> Integer -> [Integer]
fasthamming' a b c =
  xs
  where
    xs =
      1 : merge (map (a*) xs)
                (merge (map (b*) xs)
                       (map (c*) xs))


-- Ex 7.6.5
genhamming :: [Integer] -> [Integer]
genhamming as =
  1 : gh as
  where
    gh [] =
      []
    gh [y] =
      map (y*) (genhamming as)
    gh (y:ys) =
      merge (map (y*) (genhamming as))
            (gh ys)


fastgenhamming :: [Integer] -> [Integer]
fastgenhamming as =
  hs
  where
    hs =
      1 : fgh as

    fgh [x] =
      map (x*) hs
    fgh (x:xs) =
      merge (map (x*) hs)
            (fgh xs)


-- forget about union types for now
type Move =
  String

type Round =
  (Move, Move)


type Strategy =
  [Move] -> [Move]


beats :: Move -> Move
beats "Rock" =
  "Paper"
beats "Paper" =
  "Scissors"
beats "Scissors" =
  "Rock"


score :: Round -> (Int, Int)
score (x, y)
  | x == y =
    (0, 0)
  | x == beats y =
    (1, 0)
  | y == beats x =
    (0, 1)


rounds :: (Strategy, Strategy) -> [Round]
rounds (f, g) =
  zip xs ys
  where
    xs =
      fair f ys

    ys =
      fair g xs


match :: Int -> (Strategy, Strategy) -> (Int, Int)
match n =
  total . map score . take n . rounds


total :: [(Int, Int)] -> (Int, Int)
total scores =
  (sum (map fst scores), sum (map snd scores))


recip :: Strategy
recip ms =
  "Paper" : ms


smart :: Strategy
smart ms =
  "Rock" : map choose counts
  where
    counts =
      tail (scanl count (0, 0, 0) ms)

    choose :: (Int, Int, Int) -> Move
    choose (r, p, s)
      | a < r =
        "Rock"
      | r <= a && a < r + p =
        "Paper"
      | r + p <= a && a < r + p + s =
        "Scissors"
      where
        a =
          (r + p + s) `div` 2 -- forget about random for now


count :: (Int, Int, Int) -> Move -> (Int, Int, Int)
count (r, p, s) "Rock" =
  (r + 1, p, s)
count (r, p, s) "Paper" =
  (r, p + 1, s)
count (r, p, s) "Scissors" =
  (r, p, s + 1)


cheat :: Strategy
cheat =
  map beats


fair :: Strategy -> [Move] -> [Move]
fair f ms =
  zs
  where
    zs =
      f (synch zs ms)

    synch (y:ys) (x:xs)
      | defined y =
        x : synch ys xs
      | otherwise =
        error $ "Not valid: " ++ y


-- Ex. 7.7.4
defined :: String -> Bool
defined "Rock" =
  True
defined "Paper" =
  True
defined "Scissors" =
  True
defined _ =
  False


-- Ex. 7.7.2
roundrobin :: Strategy
roundrobin _ms =
  "Rock" : map move [2..]


leastfreq :: Strategy
leastfreq ms =
  "Paper" : map raremove freqs
  where
    freqs =
      tail (scanl count (0, 0, 0) ms)

    raremove (r, p, s)
      | minfreq == r =
        "Rock"
      | minfreq == p =
        "Paper"
      | minfreq == s =
        "Scissors"
      where
        minfreq =
          minimum [r, p, s]


move :: Int -> String
move e
  | e `mod` 3 == 0 =
      "Scissors"
  | e `mod` 3 == 1 =
      "Paper"
  | e `mod` 3 == 2 =
      "Rock"


third :: Strategy
third ms =
  "Scissors" : map lookthird (zip [2..] ms)
  where
    lookthird (n, m)
      | n `mod` 3 == 0 =
        beats m
      | otherwise =
        move n


-- Execute: `match 11 (recip, sneakycheat)`. The execution will never go past the 10th move.
sneakycheat :: Strategy
sneakycheat ms =
  "Rock" : take 9 (leastfreq ms) ++ cheat (drop 10 ms)
