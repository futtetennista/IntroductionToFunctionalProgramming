module Countdown
where



data Expr
  = Val Int
  | App Op Expr Expr


instance Show Expr where
  show (Val x) =
    show x

  show (App op x y) =
    "(" ++ show x ++ show op ++ show y ++ ")"


data Op
  = Add
  | Sub
  | Mul
  | Div


instance Show Op where
  show Add =
    " + "
  show Sub =
    " - "
  show Mul =
    " * "
  show Div =
    " / "


solutions :: [Int] -> Int -> [Expr]
solutions xs n =
  [e | ys <- choices xs, e <- exprs ys, eval e == [n]]


choices :: [Int] -> [[Int]]
choices ns =
  -- concat . (map perms) . subs
  [ys | xs <- subs ns, ys <- perms xs]
  -- [ys | yss <- map perms (subs ns) , ys <- yss]
  where
    subs :: [a] -> [[a]]
    subs [] =
      [[]]
    subs (x:xs) =
      subs xs ++ map (x:) (subs xs)

    perms :: [a] -> [[a]]
    perms [] =
      [[]]
    perms (x:xs) =
      -- concat (map (interleave x) (perms xs))
      [zs | ys <- perms xs, zs <- interleave x ys]

    interleave :: a -> [a] -> [[a]]
    interleave x [] =
      [[x]]
    interleave x (y:ys) =
      (x:y:ys) : map (y:) (interleave x ys)


exprs :: [Int] -> [Expr]
exprs [] =
  []
exprs [x] =
 [Val x]
exprs xs@(_:_) =
  [e | (ls, rs) <- split xs
     , l <- exprs ls
     , r <- exprs rs
     , e <- combine l r]
  where
    combine :: Expr -> Expr -> [Expr]
    combine l r =
      [App op l r | op <- ops]


ops :: [Op]
ops =
  [Add, Sub, Mul, Div]


split :: [a] -> [([a], [a])]
split [] =
  []
-- must return [] to avoid having duplicates in the result, since the definition for lists with 2+ elements already returns amongs its results smth like ([x], [])
split [_] =
  []
split (x:xs) =
  ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]


valid :: Op -> Int -> Int -> Bool
valid Sub x y =
  x > y
valid Div x y =
  x `mod` y == 0
valid _ _ _ =
  True


eval :: Expr -> [Int]
eval (Val x) =
  [x | x > 0]
eval (App op x y) =
  [apply op x' y' | x' <- eval x
                  , y' <- eval y
                  , valid op x' y']


apply :: Op -> Int -> Int -> Int
apply op x y =
  f x y
  where
    f =
      nakedOp op

    nakedOp :: Op -> (Int -> Int -> Int)
    nakedOp Add =
      (+)
    nakedOp Sub =
      (-)
    nakedOp Mul =
      (*)
    nakedOp (Div) =
      div


solutions' :: [Int] -> Int -> [Expr]
solutions' xs n =
  [e | ys <- choices xs, (e, res) <- exprs' valid ys, res == n]


type Result =
  (Expr, Int)


type OpValidator =
  Op -> Int -> Int -> Bool


exprs' :: OpValidator -> [Int] -> [Result]
exprs' _ [] =
  []
exprs' _ [x] =
  [(Val x, x) | x > 0]
exprs' f xs@(_:_) =
  [res | (ls, rs) <- split xs
       , l <- exprs' f ls
       , r <- exprs' f rs
       , res <- combine l r]
  where
    combine :: Result -> Result -> [Result]
    combine (e1, i1) (e2, i2) =
      [(App op e1 e2, apply op i1 i2) | op <- ops, f op i1 i2]


fastsolutions :: [Int] -> Int -> [Expr]
fastsolutions xs n =
  [e | ys <- choices xs, (e, res) <- exprs' valid' ys, res == n]


valid' :: Op -> Int -> Int -> Bool
-- x + y == y + x
valid' Add x y =
  x <= y
valid' Sub x y =
  x > y
-- 1 * x == x * 1, x * y == y * x
valid' Mul x y =
  x /= 1 && y /= 1 && x <= y
-- x / 1 = x
valid' Div x y =
  y /= 1 && x `mod` y == 0
