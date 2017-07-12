{-# LANGUAGE OverloadedStrings #-}

module MonadTransformers
where

import Prelude hiding (abs, exp)
import Control.Applicative ((<|>))
import qualified Control.Monad.Identity as MId
-- import qualified Control.Monad.Trans.Except as ME
import qualified Control.Monad.Except as MEx
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW
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
      Map.insert var val env'

    FunVal env' var bodyexp =
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
  FunVal env' var bodyexp <- eval1 env e1
  val <- eval1 env e2
  eval1 (Map.insert var val env') bodyexp


type EvalFail =
  MEx.ExceptT T.Text MId.Identity


type Eval2 a =
  EvalFail a


runEval2 :: Eval2 a -> Either T.Text a
runEval2 x =
  MId.runIdentity (MEx.runExceptT x)


eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) =
  return (IntVal i)
eval2 env (Var x) = do
  case Map.lookup x env of
    Nothing ->
      MEx.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
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
          MEx.throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval2 env (Abs x e) =
  return $ FunVal env x e
eval2 env (App e1 e2) = do
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval2 env e2
  eval1 (Map.insert var val env') bodyexp
  where
    evalFunOrFail = do
      res <- eval2 env e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          MEx.throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


-- Hide the environment
type Eval3 a =
  MR.ReaderT Env EvalFail a


runEval3 :: Env -> Eval3 a -> Either T.Text a
runEval3 env x =
  MId.runIdentity (MEx.runExceptT (MR.runReaderT x env))


eval3 :: Exp -> Eval3 Value
eval3 (Lit i) =
  return $ IntVal i
eval3 (Var x) = do
  mexp <- MR.asks (Map.lookup x)
  case mexp of
    Nothing ->
      -- The mtl machinery makes it so that lift-ing is done under the hood:
      -- MR.lift $ MEx.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
      MEx.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")

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
          MEx.throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval3 (Abs x e) =
  do env <- MR.ask ; return $ FunVal env x e
eval3 (App e1 e2) = do
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval3 e2
  let env'' = Map.insert var val env'
  MR.local (const env'') (eval3 bodyexp)
  where
    evalFunOrFail = do
      res <- eval3 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          MEx.throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


-- STATE
-- Don't keep track of the state if a failure occurres
type Eval4 a =
  MR.ReaderT Env (MS.StateT Int (MEx.ExceptT T.Text MId.Identity)) a


-- Keep track of the state even if a failure occures
type Eval4' a =
  MR.ReaderT Env (MEx.ExceptT T.Text (MS.StateT State MId.Identity)) a


type State =
  Int


runEval4 :: Env -> State -> Eval4 a -> Either T.Text (a, Int)
runEval4 env st x =
  MId.runIdentity . MEx.runExceptT . MS.runStateT (MR.runReaderT x env) $ st


runEval4' :: Env -> State -> Eval4' a -> (Either T.Text a, Int)
runEval4' env st x =
  MId.runIdentity . MS.runStateT (MEx.runExceptT (MR.runReaderT x env)) $ st


tick :: (Num a, MS.MonadState a m) => m ()
tick =
  do n <- MS.get ; MS.put (n + 1)


eval4 :: Exp -> Eval4 Value
eval4 (Lit i) =
  do tick ; return $ IntVal i
eval4 (Var x) = do
  tick
  mexp <- MR.asks (Map.lookup x)
  maybe throwError return mexp
  where
    throwError =
      MEx.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
eval4 (Add e1 e2) = do
  tick
  x <- intValOrFail e1
  y <- intValOrFail e2
  return $ IntVal (x + y)
  where
    intValOrFail x = do
      res <- eval4 x
      case res of
        IntVal y ->
          return y

        _ ->
          MEx.throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval4 (Abs x e) =
  do tick ; env <- MR.ask ; return $ FunVal env x e
eval4 (App e1 e2) = do
  tick
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval4 e2
  let env'' = Map.insert var val env'
  MR.local (const env'') (eval4 bodyexp)
  where
    evalFunOrFail = do
      res <- eval4 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          MEx.throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


-- Add some logging
type Eval5 a =
  MR.ReaderT Env (MW.WriterT [T.Text] (MS.StateT Int (MEx.ExceptT T.Text MId.Identity))) a


type Eval5' a =
  MR.ReaderT Env (MEx.ExceptT T.Text (MW.WriterT [T.Text] (MS.StateT Int MId.Identity))) a


type Eval5'' a =
  MR.ReaderT Env (MEx.ExceptT T.Text (MS.StateT Int (MW.WriterT [T.Text] MId.Identity))) a


runEval5 :: Env -> State -> Eval5 a -> Either T.Text ((a, [T.Text]), Int)
runEval5 env st x =
  MId.runIdentity . MEx.runExceptT . MS.runStateT (MW.runWriterT (MR.runReaderT x env)) $ st


runEval5' :: Env -> State -> Eval5' a -> ((Either T.Text a, [T.Text]), Int)
runEval5' env st x =
  MId.runIdentity (MS.runStateT (MW.runWriterT . MEx.runExceptT $ MR.runReaderT x env) st)


runEval5'' :: Env -> State -> Eval5'' a -> ((Either T.Text a, Int), [T.Text])
runEval5'' env st x =
  MId.runIdentity . MW.runWriterT . MS.runStateT (MEx.runExceptT (MR.runReaderT x env)) $ st


eval5 :: Exp -> Eval5 Value
eval5 (Lit i) =
  do tick ; MW.tell [T.pack ("lit " ++ show i)] ; return $ IntVal i
eval5 (Var x) = do
  tick
  mexp <- MR.asks (Map.lookup x)
  case mexp of
    Nothing ->
      MEx.throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")

    Just e ->
      do MW.tell [T.pack ("var " ++ show x ++ "=" ++ show e)] ; return e
eval5 (Add e1 e2) = do
  tick
  x <- intValOrFail e1
  y <- intValOrFail e2
  MW.tell [T.pack ("add: " ++ show x ++ " " ++ show y)]
  return $ IntVal (x + y)
  where
    intValOrFail x = do
      res <- eval5 x
      case res of
        IntVal y ->
          return y

        _ ->
          MEx.throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval5 (Abs x e) =
  do tick ; MW.tell [T.pack ("lambda abs")] ; env <- MR.ask ; return $ FunVal env x e
eval5 (App e1 e2) = do
  tick
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval5 e2
  let env'' = Map.insert var val env'
  MW.tell [T.pack ("fun app: " ++ show var ++ " " ++ show val)]
  MR.local (const env'') (eval5 bodyexp)
  where
    evalFunOrFail = do
      res <- eval5 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          MEx.throwError (T.pack $ "'" ++ show res ++ "' should have type fun")
