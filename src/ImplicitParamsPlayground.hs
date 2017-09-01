{-# LANGUAGE ImplicitParams #-}
module ImplicitParamsPlayground
where

import MonadTransformersPlayground
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import qualified Data.Text as T
import qualified Data.Map as Map


-- This works
runEval2' :: Eval2 a -> Either T.Text a
runEval2' =
  runIdentity . runExceptT . unwrapEvalFail


eval2' :: Env -> Exp -> Eval2 Value
eval2' env =
  let ?env = env in eval2


-- This doesn't work
runEval2 :: Env -> Eval2 a -> Either T.Text a
runEval2 env ev2 =
  -- let ?env = env in runIdentity . runExceptT $ unwrapEvalFail ev2
  runIdentity . runExceptT $ unwrapEvalFail ev2'
  where
    ev2' =
      let ?env = env in ev2


eval2 :: (?env :: Env) => Exp -> Eval2 Value
eval2 (Lit i) =
  return (IntVal i)
eval2 (Var x) =
  maybe mthrow return (Map.lookup x ?env)
  where
    mthrow =
      throwError . T.pack $ "Unbound variable '" ++ show x ++ "'"
eval2 (Add e1 e2) =
  IntVal <$> ((+) <$> intValOrFail e1 <*> intValOrFail e2)
  where
    intValOrFail x = do
      res <- eval2 x
      case res of
        IntVal y ->
          return y

        _ ->
          throwError (T.pack $ "'" ++ show x ++ "' should have type int" )
eval2 (Abs x e) =
  return (FunVal ?env x e)
eval2 (App e1 e2) = do
  (var, bodyexp, env') <- evalFunOrFail
  val <- eval2  e2
  let ?env = Map.insert var val env'
  eval2 bodyexp
  where
    evalFunOrFail = do
      res <- eval2 e1
      case res of
        FunVal env' var bodyexp ->
          return (var, bodyexp, env')

        _ ->
          throwError (T.pack $ "'" ++ show res ++ "' should have type fun")
