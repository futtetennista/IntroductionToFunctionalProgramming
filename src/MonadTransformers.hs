{-# LANGUAGE OverloadedStrings #-}

module MonadTransformers
where

import Prelude hiding (abs, exp)
import Control.Applicative ((<|>), many, some)
import qualified Control.Monad.Identity as MId
-- import qualified Control.Monad.Trans.Except as ME
import qualified Control.Monad.Except as MExc
import qualified Control.Monad.Reader as MR
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Maybe as M
import qualified Calculator as P


type Name =
  T.Text


data Exp
  = Lit Int
  | Var Name
  | Add Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving Show


data Value
  = IntVal Int
  | FunVal Env Name Exp
  deriving Show


type Env =
  Map.Map Name Value


-- PARSING
{--
Grammar:
exp  ::= term (+ exp | nil)
term ::= fun | fact
fun  ::= (abs) exp | abs
abs  ::= \name -> exp
name ::= alphanum | _
fact ::= (exp) | nat | name
nat  ::= … | -1 | 0 | 1 | …
--}

sampleInput :: T.Text
sampleInput =
  "12 + ((\\x -> x) (4 + 2))"

fact :: P.Parser Exp
fact =
  parens exp <|> name <|> nat
  where
    nat =
      do n <- P.natural ; return (Lit n)


parens :: P.Parser a -> P.Parser a
parens p =
  do _ <- P.symbol "(" ; e <- p ; _ <- P.symbol ")" ; return e


name :: P.Parser Exp
name =
  do xs <- P.var ; return (Var xs)


abs :: P.Parser Exp
abs = do
  _ <- P.symbol "\\"
  Var n <- name
  _ <- P.symbol "->"
  e <- exp
  return (Abs n e)


fun :: P.Parser Exp
fun =
  App <$> parens abs <*> exp <|> abs


term :: P.Parser Exp
term =
  fun <|> fact


exp :: P.Parser Exp
exp =
  do e1 <- term ; add e1 <|> return e1
  where
    add x =
      do _ <- P.symbol "+" ; y <- exp ; return (Add x y)


-- EVALUATION
-- No monads
eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) =
  IntVal i
eval0 env (Var x) =
  M.fromJust (Map.lookup x env) -- can 💥
eval0 env (Add e1 e2) =
  IntVal (x + y)
  where
    IntVal x =
      eval0 env e1 -- can 💥

    IntVal y =
      eval0 env e2 -- can 💥
eval0 env (Abs x e) =
  FunVal env x e
eval0 env (App e1 e2) =
  eval0 env'' bodyexp
  where
    env'' =
      Map.insert funname val env'

    FunVal env' funname bodyexp =
      eval0 env e1 -- can 💥

    val =
      eval0 env e2


type Eval1 a =
  MId.Identity a


runEval1 :: Eval1 a -> a
runEval1 =
  MId.runIdentity


eval1 :: Monad m => Env -> Exp -> m Value
eval1 _ (Lit i) =
  return (IntVal i)
eval1 env (Var x) = do
  return $ M.fromJust (Map.lookup x env)
eval1 env (Add e1 e2) = do
  IntVal x <- eval1 env e1
  IntVal y <- eval1 env e2
  return $ IntVal (x + y)
eval1 env (Abs x e) =
  return $ FunVal env x e
eval1 env (App e1 e2) = do
  FunVal env' funname bodyexp <- eval1 env e1
  val <- eval1 env e2
  eval1 (Map.insert funname val env') bodyexp


type EvalFail =
  MExc.ExceptT T.Text MId.Identity


type Eval2 a =
  EvalFail a


runEval2 :: Eval2 a -> Either T.Text a
runEval2 x =
  MId.runIdentity (MExc.runExceptT x)


eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) =
  return (IntVal i)
eval2 env (Var x) = do
  case Map.lookup x env of
    Nothing ->
      MExc.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
      -- throwUnboundVarError x

    Just e ->
      return e
eval2 env (Add e1 e2) = do
  x <- intValOrFail e1
  y <- intValOrFail e2
  return $ IntVal (x + y)
  where
    intValOrFail x = do
      res <- eval2 env x
      case res of
        IntVal y ->
          return y

        _ ->
          MExc.throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval2 env (Abs x e) =
  return $ FunVal env x e
eval2 env (App e1 e2) = do
  (funname, bodyexp, env') <- evalFunOrFail
  val <- eval2 env e2
  eval1 (Map.insert funname val env') bodyexp
  where
    evalFunOrFail = do
      res <- eval2 env e1
      case res of
        FunVal env' funname bodyexp ->
          return (funname, bodyexp, env')

        _ ->
          MExc.throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


-- Hide the environment
type Eval3 a =
  MR.ReaderT Env EvalFail a


runEval3 :: Env -> Eval3 a -> Either T.Text a
runEval3 env x =
  MId.runIdentity (MExc.runExceptT (MR.runReaderT x env))


eval3 :: Exp -> Eval3 Value
eval3 (Lit i) =
  return $ IntVal i
eval3 (Var x) = do
  mexp <- MR.asks (Map.lookup x)
  case mexp of
    Nothing ->
      MExc.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")

    Just e ->
      return e
eval3 (Add e1 e2) = do
  x <- intValOrFail e1
  y <- intValOrFail e2
  return $ IntVal (x + y)
  where
    intValOrFail x = do
      res <- eval3 x
      case res of
        IntVal y ->
          return y

        _ ->
          MExc.throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval3 (Abs x e) =
  do env <- MR.ask ; return $ FunVal env x e
eval3 (App e1 e2) = do
  (funname, bodyexp, env') <- evalFunOrFail
  val <- eval3 e2
  MR.local (const (Map.insert funname val env')) (eval3 bodyexp)
  where
    evalFunOrFail = do
      res <- eval3 e1
      case res of
        FunVal env' funname bodyexp ->
          return (funname, bodyexp, env')

        _ ->
          MExc.throwError (T.pack $ "'" ++ show res ++ "' should have type fun")
