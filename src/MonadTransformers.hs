{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadTransformers ( Exp(..)
                         , Value(..)
                         , Eval2
                         , Env
                         , unwrapEvalFail
                         )
where

import Prelude hiding (abs, exp)
import Control.Applicative ((<|>), liftA)
import Control.Monad.Identity (Identity, runIdentity)
import Text.Parsec hiding ((<|>), State)
-- import qualified Control.Monad.Trans.Except as ME
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError, catchError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)
import Control.Monad.Writer (MonadWriter, WriterT, tell, runWriterT)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Maybe as M


{-
Interesting example from https://www.reddit.com/r/haskell/comments/6w4kml/weekly_beginner_saturday_hask_anything_1/dm6hqmv/

runExceptT $ runStateT (example0 :: StateT Int (ExceptT String Identity) ()) 0 == ?
[example0#put]        Right ((), 1)
  [example1#put]        Right ((), 2)
  [example1#throwError] Left "Urk!" -- will be discarded
[example0#catchError] Right ((), 1)

runStateT (runExceptT (example0 :: ExceptT String (StateT Int Identity) ())) 0 == ?
[example0#put]        (Right (), 1)
  [example1#put]        (Right (), 2)
  [example1#throwError] (Left "Urk!", 2) -- (Left "Urk!") will be discarded
[example0#catchError] (Right (), 2)
-}
example0 :: (MonadError String m, MonadState Int m) => m ()
example0 = do
  put 1
  example1 `catchError` (\_ -> return ())


example1 :: (MonadError String m, MonadState Int m) => m ()
example1 = do
  put 2
  throwError "Urk!"


-- Code taken from the tutorial: "Monad Transformers Step by Step" by Martin Grabmüller
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


sampleExp :: Exp
sampleExp =
  either undefined id (parse exp "(sample)" sampleInput)


fact :: Parsec T.Text () Exp
fact =
  parens exp <|> name <|> nat
  where
    nat =
      Lit <$> (liftA read natural)

    natural =
      (++) <$> option "" (string "-") <*> many1 digit


parens :: Parsec T.Text u a -> Parsec T.Text u a
parens =
  between (char '(') (char ')')


name :: Parsec T.Text () Exp
name =
  Var <$> var
  where
    var =
      liftA T.pack ((:) <$> lower <*> many alphaNum)


abs :: Parsec T.Text () Exp
abs = -- do
  -- _ <- string "\\"
  -- Var n <- name
  -- _ <- string "->"
  -- e <- exp
  -- return (Abs n e)
  -- string "\\" *>
  (\(Var n) -> Abs n) <$ string "\\" <*> name <* string "->" <*> exp


fun :: Parsec T.Text () Exp
fun =
  App <$> parens abs <*> exp <|> abs


term :: Parsec T.Text () Exp
term =
  fun <|> fact


exp :: Parsec T.Text () Exp
exp =
  do e1 <- term ; add e1 <|> return e1
  where
    add x =
      pure (Add x) <*> (char '+' *> exp)
      -- do _ <- char '+' ; y <- exp ; return (Add x y)


-- EVALUATION
-- No monads
eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) =
  IntVal i
eval0 env (Var x) =
  M.fromJust (Map.lookup x env) -- can fail
eval0 env (Add e1 e2) =
  IntVal (x + y)
  where
    IntVal x =
      eval0 env e1 -- can fail

    IntVal y =
      eval0 env e2 -- can fail
eval0 env (Abs x e) =
  FunVal env x e
eval0 env (App e1 e2) =
  eval0 env'' bodyexp
  where
    env'' =
      Map.insert var val env'

    FunVal env' var bodyexp =
      eval0 env e1 -- can fail

    val =
      eval0 env e2


type Eval1 a =
  Identity a


runEval1 :: Eval1 a -> a
runEval1 =
  runIdentity


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


newtype EvalFail a =
  EvalFail (ExceptT T.Text Identity a)
  deriving (Functor, Applicative, Monad, MonadError T.Text)


type Eval2 a =
  EvalFail a


unwrapEvalFail :: EvalFail a -> ExceptT T.Text Identity a
unwrapEvalFail (EvalFail x) =
  x


runEval2 :: Eval2 a -> Either T.Text a
runEval2 =
  runIdentity . runExceptT . unwrapEvalFail


eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) =
  return (IntVal i)
eval2 env (Var x) = do
  case Map.lookup x env of
    Nothing ->
      throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
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
          throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval2 env (Abs x e) =
  return $ FunVal env x e
eval2 env (App e1 e2) = do
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval2 env e2
  eval2 (Map.insert var val env') bodyexp
  where
    evalFunOrFail = do
      res <- eval2 env e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


sampleRunEval2 :: Either T.Text Value
sampleRunEval2 =
  runEval2 (eval2 Map.empty sampleExp)


-- Hide the environment
newtype Eval3 a =
  Eval3 (ReaderT Env EvalFail a)
  deriving (Functor, Applicative, Monad, MonadError T.Text, MonadReader Env)


unwrapEval3 :: Eval3 a -> ReaderT Env EvalFail a
unwrapEval3 (Eval3 x) =
  x


runEval3 :: Env -> Eval3 a -> Either T.Text a
runEval3 env x =
  runIdentity . runExceptT . unwrapEvalFail $ runReaderT (unwrapEval3 x) env


eval3 :: Exp -> Eval3 Value
eval3 (Lit i) =
  return $ IntVal i
eval3 (Var x) = do
  mexp <- asks (Map.lookup x)
  case mexp of
    Nothing ->
      -- The mtl machinery makes it so that lift-ing is done under the hood:
      -- lift $ throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
      throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")

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
          throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval3 (Abs x e) =
  do env <- ask ; return $ FunVal env x e
eval3 (App e1 e2) = do
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval3 e2
  let env'' = Map.insert var val env'
  local (const env'') (eval3 bodyexp)
  where
    evalFunOrFail = do
      res <- eval3 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


sampleRunEval3 :: Either T.Text Value
sampleRunEval3 =
  runEval3 Map.empty (eval3 sampleExp)


-- STATE
-- Don't keep track of the state if a failure occurres
type Eval4 a =
  ReaderT Env (StateT Int (ExceptT T.Text Identity)) a


-- Keep track of the state even if a failure occures
type Eval4' a =
  ReaderT Env (ExceptT T.Text (StateT State Identity)) a


type State =
  Int


runEval4 :: Env -> State -> Eval4 a -> Either T.Text (a, Int)
runEval4 env st x =
  runIdentity . runExceptT . runStateT (runReaderT x env) $ st


runEval4' :: Env -> State -> Eval4' a -> (Either T.Text a, Int)
runEval4' env st x =
  runIdentity . runStateT (runExceptT (runReaderT x env)) $ st


tick :: (Num a, MonadState a m) => m ()
tick =
  do n <- get ; put (n + 1)


eval4 :: Exp -> Eval4 Value
eval4 (Lit i) =
  tick >> return (IntVal i)
eval4 (Var x) = do
  tick
  mexp <- asks (Map.lookup x)
  maybe mthrow return mexp
  where
    mthrow =
      throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")
eval4 (Add e1 e2) =
  tick >> IntVal <$> ((+) <$> intValOrFail e1 <*> intValOrFail e2)
  -- intValOrFail e1 >>= \x -> intValOrFail e2 >>= return . IntVal . (x+)
  -- x <- intValOrFail e1
  -- y <- intValOrFail e2
  -- return $ IntVal (x + y)
  where
    intValOrFail :: Exp -> Eval4 Int
    intValOrFail x = do
      res <- eval4 x
      case res of
        IntVal y ->
          return y

        _ ->
          throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval4 (Abs x e) =
  do tick ; env <- ask ; return $ FunVal env x e
eval4 (App e1 e2) = do
  tick
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval4 e2
  let env'' = Map.insert var val env'
  local (const env'') (eval4 bodyexp)
  where
    evalFunOrFail = do
      res <- eval4 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          throwError (T.pack $ "'" ++ show res ++ "' should have type fun")


-- Add some logging
type Eval5 a =
  ReaderT Env (WriterT [T.Text] (StateT Int (ExceptT T.Text Identity))) a


type Eval5' a =
  ReaderT Env (ExceptT T.Text (WriterT [T.Text] (StateT Int Identity))) a


type Eval5'' a =
  ReaderT Env (ExceptT T.Text (StateT Int (WriterT [T.Text] Identity))) a


type Eval5''' =
  ReaderT Env (StateT Int (WriterT [T.Text] (ExceptT T.Text Identity)))


runEval5 :: Env -> State -> Eval5 a -> Either T.Text ((a, [T.Text]), Int)
runEval5 env st evl =
  runIdentity . runExceptT . runStateT (runWriterT (runReaderT evl env)) $ st


runEval5' :: Env -> State -> Eval5' a -> ((Either T.Text a, [T.Text]), Int)
runEval5' env st evl =
  runIdentity (runStateT (runWriterT . runExceptT $ runReaderT evl env) st)


runEval5'' :: Env -> State -> Eval5'' a -> ((Either T.Text a, Int), [T.Text])
runEval5'' env st evl =
  runIdentity . runWriterT . runStateT (runExceptT (runReaderT evl env)) $ st


runEval5''' :: Env -> State -> Eval5''' a -> Either T.Text ((a, Int), [T.Text])
runEval5''' env st evl =
  runIdentity . runExceptT . runWriterT . runStateT (runReaderT evl env) $ st


eval5 :: Exp -> Eval5 Value
eval5 (Lit i) =
  do tick ; tell [T.pack ("lit " ++ show i)] ; return $ IntVal i
eval5 (Var x) = do
  tick
  mexp <- asks (Map.lookup x)
  case mexp of
    Nothing ->
      throwError (T.pack  $ "Unbound variable '" ++ show x ++ "'")

    Just e ->
      do tell [T.pack ("var " ++ show x ++ "=" ++ show e)] ; return e
eval5 (Add e1 e2) = do
  tick
  x <- intValOrFail e1
  y <- intValOrFail e2
  tell [T.pack ("add: " ++ show x ++ " " ++ show y)]
  return $ IntVal (x + y)
  where
    intValOrFail x = do
      res <- eval5 x
      case res of
        IntVal y ->
          return y

        _ ->
          throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval5 (Abs x e) =
  do tick ; tell [T.pack ("lambda abs")] ; env <- ask ; return $ FunVal env x e
eval5 (App e1 e2) = do
  tick
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval5 e2
  let env'' = Map.insert var val env'
  tell [T.pack ("fun app: " ++ show var ++ " " ++ show val)]
  local (const env'') (eval5 bodyexp)
  where
    evalFunOrFail = do
      res <- eval5 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          throwError (T.pack $ "'" ++ show res ++ "' should have type fun")
