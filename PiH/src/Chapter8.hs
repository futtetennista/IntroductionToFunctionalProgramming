{-# LANGUAGE OverloadedStrings #-}
module Chapter8

where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

default (T.Text)


data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show


-- Ex. 8.3
balanced :: Tree a -> Bool
balanced (Leaf _) =
  True
balanced (Node l r) =
  abs (size l - size r) <= 1
  where
    size :: Tree a -> Int
    size (Leaf _) =
      1
    size (Node l' r') =
      size l' + size r'


-- Ex. 8.4
-- Gotcha: this balances only complete trees, given the definition of Tree
balance :: [a] -> Either (T.Text) (Tree a)
balance xs
  | length xs `mod` 2 /= 0 =
    Left $ "Array length must be a power of 2 for the tree to be complete"
  | otherwise =
    balance' xs
    where
      balance' [y] =
        Right $ Leaf y
      balance' ys@(_:_) =
        Node <$> (balance' ls) <*> (balance' rs)
        where
          (ls, rs) =
            split ys

          split zs =
            splitAt (max 1 (length zs `div` 2)) zs


-- Ex. 8.5
data Expr
  = Val Int
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving Show


folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) =
  f x
folde f g (Add x y) =
  g (folde f g x) (folde f g y)
folde f g (Mul x y) =
  g (folde f g x) (folde f g y)


-- Ex. 8.6
evale :: Expr -> Int
evale =
  folde id (+)


testEval :: IO ()
testEval =
  putStrLn $ "eval " ++ show expr ++ " = " ++ show (evale expr)
  where
    expr =
      Add (Add (Val 3) (Add (Val 11) (Val 23))) (Add (Val 7) (Val 6))


sizee :: Expr -> Int
sizee (Val _) =
  1
sizee (Add x y) =
  sizee x + sizee y
sizee (Mul x y) =
  sizee x + sizee y


testSize :: IO ()
testSize =
  putStrLn $ "eval " ++ show expr ++ " = " ++ show (sizee expr)
  where
    expr =
      Add (Add (Val 3) (Add (Val 11) (Val 23))) (Add (Val 7) (Val 6))


-- Ex. 8.8
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Equiv Prop Prop
  deriving Show


type Subst =
  Map.Map Char Bool


evalp :: Subst -> Prop -> Bool
evalp _ (Const x) =
  x
evalp s (Var c) =
  maybe (error $ "Unknown var " ++ show c) id (Map.lookup c s)
evalp s (Not p) =
  not (evalp s p)
evalp s (And p1 p2) =
  evalp s p1 && evalp s p2
evalp s (Imply p1 p2) =
  evalp s p1 <= evalp s p2
evalp s (Or p1 p2) =
  evalp s p1 || evalp s p2
evalp s (Equiv p1 p2) =
  evalp s p1 == evalp s p2


vars :: Prop -> [Char]
vars (Const _) =
  []
vars (Var c) =
  [c]
vars (Not x) =
  vars x
vars (And p1 p2) =
  vars p1 ++ vars p2
vars (Imply p1 p2) =
  vars p1 ++ vars p2
vars (Or p1 p2) =
  vars p1 ++ vars p2
vars (Equiv p1 p2) =
  vars p1 ++ vars p2


vars' :: Prop -> [Char]
vars' x =
  varsHelp x []
  where
    varsHelp (Const _) acc =
      acc
    varsHelp (Var c) acc =
      c : acc
    varsHelp (Not e) acc =
      varsHelp e acc
    varsHelp (And e1 e2) acc =
      varsHelp e2 (varsHelp e1 acc)
    varsHelp (Imply e1 e2) acc =
      varsHelp e2 (varsHelp e1 acc)
    varsHelp (Or e1 e2) acc =
      varsHelp e2 (varsHelp e1 acc)
    varsHelp (Equiv e1 e2) acc =
      varsHelp e2 (varsHelp e1 acc)


bools :: Int -> [[Bool]]
bools 0 =
  [[]]
bools n =
  -- map (True:) (bools (n - 1)) ++ map (False:) (bools (n - 1))
  -- concat . map (\xs -> [True:xs, False:xs]) $ bools (n - 1)
  concat [[True:xs, False:xs] | xs <- bools (n - 1)]


substs :: Prop -> [Subst]
substs p =
  map subst $ bools (length uniqueVars)
  where
    subst bs =
      -- foldr (uncurry Map.insert) Map.empty . zip (Set.toList uniqueVars)
      fst $ Set.fold (\x (s, (y:ys)) -> (Map.insert x y s, ys)) (Map.empty, bs) uniqueVars

    uniqueVars =
      Set.fromList (vars p)


isTaut :: Prop -> Bool
isTaut p =
  and [evalp s p | s <- substs p]


-- Ex. 8.9
value :: Expr -> Int
value (Val n) =
  n
value (Add x y) =
  value x + value y
value (Mul x y) =
  value x * value y
value (Div x y) =
  value x `div` value y


type Cont =
  [Op]


data Op
  = EVAL (Int -> Op) Expr
  | ADD Int
  | MUL Int
  | DIV Int


evale' :: Expr -> Cont -> Int
evale' (Val n) c =
  exece c n
evale' (Add x y) c =
  evale' x ((EVAL ADD y) : c) -- eval left to right
  -- evale' y ((EVAL ADD x) : c) -- eval right to left
evale' (Mul x y) c =
  evale' x ((EVAL MUL y) : c)
evale' (Div x y) c =
  evale' x ((EVAL DIV y) : c)
  -- [GOTCHA] change also: exece (DIV n : c) m = exece c (m `div` n)
  -- evale' y ((EVAL DIV x) : c)


exece :: Cont -> Int -> Int
exece [] n =
  n
exece (EVAL op x : c) m =
  evale' x ((op m) : c)
exece (ADD n : c) m =
  exece c (n + m)
exece (MUL n : c) m =
  exece c (n * m)
exece (DIV n : c) m =
  exece c (n `div` m)
