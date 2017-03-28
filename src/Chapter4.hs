module Chapter4()
where


-- 4.1
units :: [String]
units =
  [ "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]


teens :: [String]
teens =
  [ "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eigthteen"
  , "nineteen"
  ]


tens :: [String]
tens =
  [ "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]


convert2 :: Int -> String
convert2 x =
  combine2 $ x `divMod` 10


combine2 :: (Int, Int) -> String
combine2 (0, 0) =
  ""

combine2 (0, u) =
  units!!(u - 1)

combine2 (1, u) =
  teens!!u

combine2 (t, 0) =
  tens!!(t - 2)

combine2 (t, u) =
  tens!!(t - 2) ++ "-" ++ units!!(u - 1)


convert3 :: Int -> String
convert3 x =
  combine3 $ x `divMod` 100


combine3 :: (Int, Int) -> String
combine3 (0, t) =
  convert2 t

combine3 (h, 0) =
  units!!(h - 1) ++ " hundred"

combine3 (h, t) =
  units!!(h - 1) ++ " hundred and " ++ convert2 t


convert6 :: Int -> String
convert6 x =
  combine6 $ x `divMod` 1000


combine6 :: (Int, Int) -> String
combine6 (0, h) =
  convert3 h

combine6 (t, 0) =
  convert3 t ++ " thousand"

combine6 (t, h) =
  convert3 t ++ " thousand" ++ link ++ convert3 h
  where
    link =
      if h < 100 then " and " else " "


-- Ex. 4.1.1
convertWithFullStop :: Int -> String
convertWithFullStop x =
  convert6 x ++ "."


-- Ex. 4.1.2
convert :: Int -> String
convert =
  convert6


-- Ex. 4.1.3
convertNatural :: Int -> String
convertNatural x
  | x < 0 =
    "minus " ++ convertWithFullStop (abs x)
  | otherwise =
    convertWithFullStop x

-- Ex. 4.1.4
type Pound =
  Int

type Pence =
  Int

convertMoney :: Int -> String
convertMoney x =
  combineMoney $ x `divMod` 100


combineMoney :: (Pound, Pence) -> String
combineMoney (0, pence) =
  convert2 pence ++ " pence"

combineMoney (pound, pence) =
  convert pound ++ pluralise ++ link ++ convert2 pence ++ " pence"
  where
    pluralise =
      if pound >= 2 then " pounds" else " pound"

    link =
      if pence == 0 then "" else " and "


-- Ex. 4.1.5
reverseConvert :: String -> Int
reverseConvert str =
  toNum . reverseConvert' str $ (0, 0, 0)
  where
    toNum (t, h, d) =
      t + h + d

    reverseConvert' [] tuple =
      tuple

    reverseConvert' str' tuple@(t, h, d) =
      case span (/=' ') str' of
        ("thousand", rest) ->
          reverseConvert' rest ((h + d) * 1000, 0, 0)

        ("hundred", rest) ->
          reverseConvert' rest (t, (d * 100), 0)

        ("and", rest) ->
          reverseConvert' rest tuple

        ("", rest) ->
          reverseConvert' (dropWhile (==' ') rest) tuple

        (s, rest) ->
          reverseConvert' rest (t, h, reverseTens s)


reverseTens :: String -> Int
reverseTens str
  | elem '-' str =
    10 * (elemIndex prefix tens + 2) + (reverseUnits $ tail postfix)
  | elem str tens =
    10 * (elemIndex str tens + 2)
  | otherwise =
    reverseTeens str
  where
    (prefix, postfix) =
      span (/= '-') str


reverseTeens :: String -> Int
reverseTeens str
  | elem str teens =
    10 + (elemIndex str teens)
  | otherwise =
    reverseUnits str


reverseUnits :: String -> Int
reverseUnits str =
  elemIndex str units + 1


elemIndex :: String -> [String] -> Int
elemIndex str xs =
  case map snd $ filter ((==str) . fst) (zip xs [0..length xs - 1]) of
    [x] ->
      x
    [] ->
      error $ "No index found for value " ++ str
    (_:_) ->
      error "Multiple indexes"


-- 4.2
type Base =
  Int

type Bigit =
  Int

type Vint =
  [Bigit]


strep :: Vint -> Vint
strep [] =
  [0]
strep xs =
  dropWhile (==0) xs


align :: Vint -> Vint -> (Vint, Vint)
align xs ys =
  case (length ys) - (length xs) of
    0 ->
      (xs, ys)
    n ->
      if n > 0 then (copy 0 n ++ xs, ys)
      else (xs, copy 0 n ++ ys)


copy :: a -> Int -> [a]
copy x times =
  [x | _ <- [1..times]]


vcompare :: (Vint -> Vint -> a) -> Vint -> Vint -> a
vcompare op xs ys =
  op axs ays
  where
    (axs, ays) =
      align xs ys


carry :: Base -> Bigit -> Vint -> Vint
carry base x (c:xs) =
  (x + c) `div` base : (x + c) `mod` base : xs


norm :: Base -> Vint -> Vint
norm base =
  strep . foldr (carry base) [0]


vadd :: Base -> Vint -> Vint -> Vint
vadd base =
  vop base (+)


vsub :: Base -> Vint -> Vint -> Vint
vsub base =
  vop base (-)


vop :: Base -> (Bigit -> Bigit -> Bigit) -> Vint -> Vint -> Vint
vop base op xs ys =
  (norm base) (zipWith op axs ays)
  where
    (axs, ays) =
      align xs ys


vnegative :: Vint -> Bool
vnegative (x:_) =
  x < 0


vnegate :: Base -> Vint -> Vint
vnegate base =
  (norm base) . map negate


vmult :: Base -> Vint -> Vint -> Vint
vmult base xs ys =
  foldr1 shiftaddPsums (psums xs ys)
  where
    shiftaddPsums xs' ys' =
      vadd base (xs' ++ [0]) ys'

    psums xs' ys' =
      map (bmul base xs') ys'


bmul :: Base -> Vint -> Bigit -> Vint
bmul base xs y =
  (norm base) . map (*y) $ xs

-- 4.3
type Word' =
  String

type Par =
  [Word']

type ColWidth =
  Int

fill :: ColWidth -> [Word'] -> [Par]
fill _ [] =
  [[]]

fill limit ws =
  [fstLine] ++ fill limit rest
  where
    fstLine =
      take n ws

    rest =
      drop n ws

    n =
      greedy

    greedy =
      -- maximum [length ys | ys <- init ws, length (unwords $ ws) <= limit]
      length (takeWhile (<= limit) (scanl countw (-1) ws)) -1
      where
        countw x word =
          x + length word + 1


-- 4.4 Turtle graphics
type State =
  (Direction, Pen, Point)
type Direction =
  Int
type Pen =
  Bool
type Point =
  (Int, Int)
type Command =
  State -> State

move :: Command
move (0, p, (x, y)) =
  (0, p, (x - 1, y))
move (1, p, (x, y)) =
  (1, p, (x, y + 1))
move (2, p, (x ,y)) =
  (2, p, (x + 1, y))
move (3, p, (x, y)) =
  (3, p, (x, y - 1))


right :: Command
right (d, pen, point) =
  ((d + 1) `mod` 4, pen, point)

-- 4.4.1
left :: Command
left (d, pen, point) =
  ((d - 1) `mod` 4, pen, point)


up :: Command
up (d, _, point) =
  (d, False, point)

down :: Command
down (d, _, point) =
  (d, True, point)


square :: Int -> [Command]
square k =
  [down] ++ concat (copy side 4) ++ [up]
  where
    side =
      copy move k ++ [right]


turtle :: [Command] -> [State]
turtle =
  scanl applyTo (0, False, (0, 0))
  where
    applyTo x f =
      f x


display :: [Command] -> String
display =
  layout . picture . trail . turtle


picture :: [Point] -> [String]
picture =
  symbolise . bitmap . sortpoints
  --symbolise . fastbitmap . sortpoints


{-|
4.4.4: worst-case example for N=5:

X----
-X---
--X--
---X-
----X

Given N points ps of size S where S=(max ps - min ps), for each point p the function has to check if p is inside S. That is its time complexity is O(N*S^2).
|-}
bitmap :: [Point] -> [[Bool]]
bitmap ps =
  [[(x, y) `inside` ps | y <- yran ps] | x <- xran ps]
  where
    inside x xs =
      or (map (==x) xs)

yran :: (Enum a, Ord a) => [(b, a)] -> [a]
yran ps =
  range (map snd ps)

xran :: (Enum a, Ord a) => [(a, b)] -> [a]
xran ps =
  range (map fst ps)

range :: (Enum a, Ord a) => [a] -> [a]
range [] =
  []
range xs =
  [minimum xs..maximum xs]


-- 4.4.7: The improvement wrt bitmap is that we don't have to iterate the entire NxN points to check if a point in the x/y range, we just need to check for inequality which takes constant time. That is the time complexity is now O(S^2) where S is defined as above.
fastbitmap :: [Point] -> [[Bool]]
fastbitmap ps =
  [[ps2 /= [] | ps2 <- splitWith snd (yran ps) ps1] | ps1 <- splitWith fst (xran ps) ps]
  where
    splitWith f xs ys =
      split (map (equals f) xs) ys

    equals f x y =
      f y == x

    split [] _ =
      []
    split (p:preds) xs =
      [takeWhile p xs] ++ split preds (dropWhile p xs)


-- 4.4.2
block :: Int -> [State]
block k =
  map down (turtle (square k))


-- 4.4.3
trail :: [State] -> [Point]
trail ss =
  [point | (_, pen, point) <- ss, pen]


layout :: [String] -> String
layout xss =
  concat [xs ++ "\n" | xs <- xss]
  -- foldr (\x acc -> x ++ "\n" ++ acc) []


-- 4.4.4
boolstr :: Bool -> String
boolstr True =
  "."
boolstr False =
  " "


symbolise :: [[Bool]] -> [String]
symbolise bss =
  [concat [boolstr b | b <- bs] | bs <- bss]


-- 4.4.5
sort :: Ord a => [a] -> [a]
sort [] =
  []
sort (p:xs) =
  sort [x | x <- xs, x < p] ++ [p] ++ sort [x | x <- xs, x >= p]


-- Assumption: dups are adjacents (list is sorted)
remdups :: Ord a => [a] -> [a]
remdups xs =
  [x | (x, y) <- zip xs (tail xs), x /= y]


sortpoints :: Ord a => [a] -> [a]
sortpoints =
  remdups . sort


squareTrail5 =
  display (square 5)
