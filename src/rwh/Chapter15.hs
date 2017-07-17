{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RWH.Chapter15 ( Supply
                     , next
                     , runSupply
                     , runRandomSupply
                     , MonadSupply (..)
                     )
where

import qualified Control.Monad.State as S
import qualified System.Random as R hiding (next)
import qualified Control.Arrow as A

newtype Supply s a =
  S (S.State [s] a)
  deriving (Functor, Applicative, Monad)


runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S x) st =
  S.runState x st


snext :: Supply s (Maybe s)
snext =
  S $ do st <- S.get
         case st of
           [] ->
             return Nothing

           (x:xs) ->
             do S.put xs ; return (Just x)


unwrapSupply :: Supply s a -> S.State [s] a
unwrapSupply (S s) =
  s


randomsIO :: R.Random a => IO [a]
randomsIO =
  R.getStdRandom $ \g -> let (a, b) = R.split g in (R.randoms a, b)


randomsIO' :: R.Random a => IO [a]
randomsIO' =
  R.getStdRandom (A.first R.randoms . R.split)


runRandomSupply :: R.Random a => IO (Maybe a)
runRandomSupply =
  fmap (fst . runSupply snext) randomsIO


-- Supply class
class (Monad m) => (MonadSupply s) m | m -> s where
  next :: m (Maybe s)
