module Chapter8
where


import Chapter7 (getint)

-- 8.2.3
data Employee
  = E { name :: String
      , sex :: Sex
      , birth :: Date
      , appointment :: Date
      }
    deriving Show

data Sex
  = Male
  | Female
  | Other
  deriving Show

data Date =
  D { year :: Int
    , month :: Int
    , day :: Int
    }
  deriving Show


served :: Date -> Int -> [Employee] -> [String]
served _today@(D y m _) duration =
  map name . filter xPlusMonths
  where
    xPlusMonths x =
      12 * (y - year (appointmentDate x)) - 1 + (12 - m) + (12 - month (appointmentDate x)) >= duration * 12


served20PlusYears :: Date -> [Employee] -> [String]
served20PlusYears today =
  served today 20


appointmentDate :: Employee -> Date
appointmentDate (E _ _ _ d) =
  d


birthDate :: Employee -> Date
birthDate (E _ _ b _) =
  b


yearsToRetirement :: Date -> Employee -> Int
yearsToRetirement _today@(D y _ _) x
  | appointmentBefore1980 =
    65 - age
  | otherwise =
    60 - age
  where
    appointmentBefore1980 =
      year (appointmentDate x) < 1980

    age =
      y - year (birthDate x)


-- Ex. 8.2.5
data Temperature
  = Celsius Int
  | Fahrenheit Int
  | Kelvin Int
  deriving Show


-- Is there a more concise way to implement this?
instance Eq Temperature where
  (Celsius x) == (Celsius y) =
    x == y
  (Fahrenheit x) == (Fahrenheit y) =
    x == y
  (Kelvin x) == (Kelvin y) =
    x == y
  ct@(Celsius _) == ft@(Fahrenheit _) =
    ct == fst (convert ft)
  ct@(Celsius _) == kt@(Kelvin _) =
    ct == snd (convert kt)
  ft@(Fahrenheit _) == ct@(Celsius _) =
    ft == snd (convert ct)
  ft@(Fahrenheit _) == kt@(Kelvin _) =
    ft == snd (convert kt)
  kt@(Kelvin _) == ct@(Celsius _) =
    kt == fst (convert ct)
  kt@(Kelvin _) == ft@(Fahrenheit _) =
    kt == snd (convert ft)


-- Is there a more concise way to implement this?
instance Ord Temperature where
  (Celsius x) <= (Celsius y) =
    x <= y
  (Fahrenheit x) <= (Fahrenheit y) =
    x <= y
  (Kelvin x) <= (Kelvin y) =
    x <= y
  ct@(Celsius _) <= ft@(Fahrenheit _) =
    ct <= fst (convert ft)
  ct@(Celsius _) <= kt@(Kelvin _) =
    ct <= snd (convert kt)
  ft@(Fahrenheit _) <= ct@(Celsius _) =
    ft <= snd (convert ct)
  ft@(Fahrenheit _) <= kt@(Kelvin _) =
    ft <= snd (convert kt)
  kt@(Kelvin _) <= ct@(Celsius _) =
    kt <= fst (convert ct)
  kt@(Kelvin _) <= ft@(Fahrenheit _) =
    kt <= snd (convert ft)


-- this API is sub-optimal since the caller needs to know the order in which the conversion appear in a pair. What's a better alternative?
convert :: Temperature -> (Temperature, Temperature)
convert (Celsius x) =
  (Fahrenheit ((x + 32) * 5 `div` 9), Kelvin z)
  where
    z =
      floor (fromIntegral x + 273.15 :: Double)
convert (Fahrenheit x) =
  (Celsius ((x * 9) `div` 5 - 32), Kelvin y)
  where
    y =
      floor ((fromIntegral x + 459.67 :: Double) * 0.5555555555555556 :: Double)
convert (Kelvin x) =
  (Celsius z, Fahrenheit y)
  where
    z =
      floor (fromIntegral x - 273.15 :: Double)

    y =
      floor (1.8 * (fromIntegral x - 273.15 :: Double) + 32.0 :: Double)

-- maybe like this ?!
data Tag
  = Celsius'
  | Fahrenheit'
  | Kelvin'
  deriving Show


convert' :: Temperature -> Tag -> Temperature
convert' ct@(Celsius _) Celsius' =
  ct
convert' (Celsius x) Fahrenheit' =
  Fahrenheit ((x + 32) * 5 `div` 9)
convert' (Celsius x) Kelvin' =
  Kelvin z
  where
    z =
      floor (fromIntegral x + 273.15 :: Double)
convert' ft@(Fahrenheit _) Fahrenheit' =
  ft
convert' (Fahrenheit x) Celsius' =
  Celsius ((x * 9) `div` 5 - 32)
convert' (Fahrenheit x) Kelvin' =
  Kelvin y
  where
    y =
      floor ((fromIntegral x + 459.67 :: Double) * 0.5555555555555556 :: Double)
convert' kt@(Kelvin _) Kelvin' =
  kt
convert' (Kelvin x) Celsius' =
  Celsius z
  where
    z =
      floor (fromIntegral x - 273.15 :: Double)
convert' (Kelvin x) Fahrenheit' =
  Fahrenheit y
  where
    y =
      floor (1.8 * (fromIntegral x - 273.15 :: Double) + 32.0 :: Double)


-- Alternatively
type Temperature' =
  (Tag, Int)


convert'' :: Temperature' -> Tag -> Temperature'
convert'' _temp _tag =
  undefined


data Expr
  = Num Int
  | Expr Expr Op Expr
  deriving Show


data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving Show


eval :: Expr -> Int
eval (Num x) =
  x
eval (Expr e1 op e2) =
  apply op (eval e1) (eval e2)


apply :: Op -> (Int -> Int -> Int)
apply Add =
  (+)
apply Sub =
  (-)
apply Mul =
  (*)
apply Div =
  div


numcount :: Expr -> Int
numcount (Num _) =
  1
numcount (Expr e1 _op e2) =
  numcount e1 + numcount e2


opcount :: Expr -> Int
opcount (Num _) =
  0
opcount (Expr e1 _ e2) =
  1 + opcount e1 + opcount e2


size :: Expr -> Int
size expr =
  opcount expr + numcount expr


-- Stack machine
data Instr
  = Load Int
  | Apply Op
  deriving Show

type Stack =
  [Int]


execute :: Instr -> Stack -> Stack
execute (Load x) xs =
  x : xs
execute (Apply op) (x:y:xs) =
  apply op y x : xs
execute (Apply _) (_) =
  error "Invalid instruction"


run :: [Instr] -> Stack -> Stack
run [] xs =
  xs
run (ins:inss) xs =
  run inss (execute ins xs)


compile :: Expr -> [Instr]
compile (Num x) =
  [Load x]
compile (Expr e1 op e2) =
  compile e1 ++ compile e2 ++ [Apply op]


-- Ex. 8.3.4
unparse :: Expr -> String
unparse (Expr e1 op e2) =
  "(" ++ unparse e1 ++ show op ++ unparse e2 ++ ")"
unparse (Num x) =
  show x


-- run (compile (parse "(6+(-11*3))")) []
parse :: String -> Expr
parse (x:xs) =
  case xs' of
    [] ->
      expr

    _ ->
      error "Invalid expression"
  where
    (expr, xs') =
      uglyparse x xs


parser :: String -> (Expr, String)
parser xs =
  (openPar xs) `then'` expr `then'` closePar
  where
    expr (c:ys)
      | '-' == c || (not . null . filter (==c) $ "1234567890") =
           (Num (getint n), rest)
      | otherwise = parser (c:ys)
      where
        parseDigit d [] =
          (d, [])
        parseDigit d rem'@(x:zs)
          | (not . null . filter (==x) $ "1234567890") =
            parseDigit (d ++ [x]) zs
          | otherwise =
            (d, rem')

        (n, rest) =
          parseDigit [c] ys


    then' ys f =
      f ys

    openPar (c:ys)
      | c == '(' = ys
      | otherwise = (c:ys)

    closePar (e, (c:ys))
      | c == ')' = (e, ys)
      | c == '+' = (Expr e Add e2, r)
      | c == '*' = (Expr e Mul e2, r)
      | otherwise = (e, (c:ys))
      where (e2, r) = parser ys


uglyparse :: Char -> String -> (Expr, String)
uglyparse c cs
  | isUnaryMinus c || isDigit c =
    (Num (getint n), zs)
  | '(' == c =
      case rems of
        [] ->
          (expr, [])

        (')':xs) ->
          (expr, xs)

        _ ->
          error "Invalid expression"
  | otherwise =
      error ("Invalid character '" ++ [c] ++ "'")
  where
    (expr, rems) =
      case ys of
        [] ->
          (e1, [])

        [')'] ->
          (e1, [])

        _ ->
          (Expr e1 op e2, ys'')

    (e1, ys) =
      uglyparse (head cs) (tail cs)
    (op, ys') =
      parseOp (head ys) (tail ys)
    (e2, ys'') =
      uglyparse (head ys') (tail ys')

    parseOp '+' =
      (,) Add
    parseOp '-'=
      (,) Sub
    parseOp '*' =
      (,) Mul
    parseOp '/'=
      (,) Div
    parseOp x =
      error ("Unsupported operation: '" ++ [x] ++ "'")

    (n, zs) =
      parseDigit [c] cs

    isUnaryMinus =
      ('-'==)

    isDigit y =
      not . null . filter (==y) $ "1234567890"

    parseDigit d [] =
      (d, [])
    parseDigit d rem'@(x:xs)
      | isDigit x =
          parseDigit (d ++ [x]) xs
      | otherwise =
          (d, rem')


-- Ex. 8.3.5
foldexpr :: (Int -> a) -> (a -> Op -> a -> a) -> Expr -> a
foldexpr f _g (Num x) =
  f x
foldexpr f g (Expr e1 op e2) =
  g (foldexpr f g e1) op (foldexpr f g e2)


eval' :: Expr -> Int
eval' =
  foldexpr id (flip apply) -- foldexpr id (\e1 op e2 -> apply op e1 e2)

size' :: Expr -> Int
size' =
  foldexpr (const 1) (\numcountl _op numcountr -> 1 + numcountl + numcountr)


unparse' :: Expr -> String
unparse' =
  foldexpr show (\left op right -> "(" ++ left ++ show op ++ right ++ ")")


-- Ex. 8.3.6
data Val
  = Nval Int
  | Oval Op
  deriving Show


preorder :: Expr -> [Val]
preorder (Num x) =
  [Nval x]
preorder (Expr e1 op e2) =
  [Oval op] ++ preorder e1 ++ preorder e2


postorder :: Expr -> [Val]
postorder (Num x) =
  [Nval x]
postorder (Expr e1 op e2) =
  postorder e1 ++ postorder e2 ++ [Oval op]


-- Ex. 8.3.7
run' :: [Instr] -> Stack -> Stack
run' inss xs =
  foldl (flip execute) xs inss


-- Ex. 8.3.9
data Expr'
  = Num' Int
  | Expr' Expr' Op Expr'
  | Neg Expr'
  deriving Show


eval'' :: Expr' -> Int
eval'' (Num' x) =
  x
eval'' (Expr' e1 op e2) =
  apply op (eval'' e1) (eval'' e2)
eval'' (Neg e) =
  0 - eval'' e


-- Ex. 8.3.10
type Env =
  String -> Int

data EnvExpr
  = EnvNum Int
  | EnvExpr EnvExpr Op EnvExpr
  | EnvNeg EnvExpr
  | EnvVar String
  deriving Show


-- evalWith (\x -> if x == "x" then 6 else undefined) (EnvExpr (EnvVar "x") Mul (EnvNum 11))
evalWith :: Env -> EnvExpr -> Int
evalWith f (EnvVar x) =
  f x
evalWith _f (EnvNum x) =
  x
evalWith f (EnvExpr e1 op e2) =
  apply op (evalWith f e1) (evalWith f e2)
evalWith f (EnvNeg x) =
  0 - evalWith f x


data RepQueue a
  = Start
  | Join (RepQueue a) a
  deriving Show


empty :: RepQueue a
empty =
  Start


join :: RepQueue a -> a -> RepQueue a
join =
  Join


peek :: RepQueue a -> Maybe a
peek (Join Start x) =
  Just x
peek (Join q _) =
  peek q
peek _ =
  Nothing


pop :: RepQueue a -> Maybe (RepQueue a)
pop (Join Start _) =
  Just Start
pop (Join q x) =
  maybe Nothing (Just . flip join x) (pop q)
pop _ =
  Nothing


-- TODO: this is not what I need, 'a' in reality is 'Container b' but I'm not sure how to write it down yet
-- class Queue a where
--   start :: a
--   join :: a -> b -> a
--   front :: a -> b
--   reduce :: a -> a
--
-- instance Queue (RepQueue a) where
--   start =
--     Start

--   join =
--     Join

--   front Start =
--     undefined
--   front (Join Start x) =
--     undefined -- x
--   front (Join rq _) =
--     front rq

--   reduce Start =
--     undefined
--   reduce (Join Start x) =
--     Start
--   reduce (Join rq x) =
--     join (reduce rq) x
