module Compiler
where

import Control.Applicative ((<|>))


data Expr
  = Val Int
  | Add Expr Expr
  deriving Show


data Op
  = Push Int
  | Plus
  deriving Show


type Stack =
  [Int]

type Code =
  [Op]


-- 1. Adding a stack
push :: Int -> Stack -> Stack
push n st =
  n:st


add :: Stack -> Stack
add (x:y:xs) =
  x + y:xs


eval :: Expr -> Stack -> Stack
-- eval (Val n) st =
--   push n st
-- eval (Add e1 e2) st =
--   add (eval e2 (eval e1 st))
eval e st =
  eval' e id st


comp :: Expr -> Code -> Code
comp (Val n) st =
  Push n : st
comp (Add x y) st =
  comp y (comp x (Plus:st))


exec :: Code -> Stack -> Stack
exec [] st =
  st
exec (Push x:xs) st =
  exec xs (x:st)
exec (Plus:xs) (x:y:st) =
  exec xs ((x + y):st)


-- 2. Continuation-passing style
type Cont =
  Stack -> Stack


-- eval'' e c st = c (eval e st)
-- Expanding the type makes it a bit easier to understand the signature
-- eval' :: Expr -> (Stack -> Stack) -> (Stack -> Stack) :: Expr -> (Stack -> Stack) -> Stack -> Stack
eval' :: Expr -> Cont -> Cont
eval' (Val n) c st =
  c (push n st)
eval' (Add x y) c st =
  -- c (add (eval' y c (eval' x c st))) -- complete non-sense
  eval' x (eval' y (c . add)) st


-- 3. Defunctionalisation
haltC :: Cont
haltC =
  id


pushC :: Int -> Cont -> Cont
pushC n c st =
  c (push n st)


addC :: Cont -> Cont
addC c st =
  c (add st)


eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c =
  pushC n c
eval'' (Add x y) c =
  -- addC c (eval'' y c (eval'' x c st)) -- complete non-sense
  eval'' x (eval'' y (addC c))


data VMCode
  = HALT
  | PUSH Int VMCode
  | ADD VMCode
  deriving Show


execC :: VMCode -> Cont
execC HALT =
  haltC
execC (PUSH n c) =
  pushC n (execC c)
execC (ADD c) =
  addC (execC c)


exec' :: VMCode -> Stack -> Stack
exec' HALT st =
  st
exec' (PUSH n c) st =
  execC (PUSH n c) st
exec' (ADD c) (x:y:st) =
  execC (ADD c) (x:y:st)


comp' :: Expr -> VMCode
comp' e =
  compHelp e HALT
  where
    compHelp :: Expr -> VMCode -> VMCode
    compHelp (Val n) c =
      PUSH n c
    compHelp (Add x y) c =
      compHelp x (compHelp y (ADD c))


-- Ex. 1
data Expr'
  = Val' Int
  | Add' Expr' Expr'
  | Throw
  | Catch Expr' Expr'
  deriving Show


data VMCode'
  = HALT'
  | PUSH' Int VMCode'
  | ADD' VMCode'
  | THROW VMCode'
  | CATCH VMCode'
  deriving Show


type Stack' =
  [Result]


type Result =
  Maybe Int


meval :: Expr' -> Result
meval (Val' n) =
  Just n
meval (Add' x y) =
  (+) <$> meval x <*> meval y
meval Throw =
  Nothing
meval (Catch x h) =
  meval x <|> meval h


comp'' :: Expr' -> VMCode'
comp'' e =
  compHelp e HALT'
  where
    compHelp :: Expr' -> VMCode' -> VMCode'
    compHelp (Val' n) c =
      PUSH' n c
    compHelp (Add' e1 e2) c =
      compHelp e1 (compHelp e2 (ADD' c))
    compHelp Throw c =
      THROW c
    compHelp (Catch e' h) c =
      compHelp e' (compHelp h (CATCH c))

exec'' :: VMCode' -> Stack' -> Stack'
exec'' HALT' st =
  st
exec'' (PUSH' n c) st =
  exec'' c (Just n : st)
exec'' (ADD' c) (x:y:st) =
  exec'' c (((+) <$> x <*> y) : st)
exec'' (THROW c) st =
  exec'' c (Nothing : st)
exec'' (CATCH c) (e:h:st) =
  exec'' c ((e <|> h) : st)
