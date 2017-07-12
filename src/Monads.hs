module Monads
where

import Control.Monad.State ((>=>))
import qualified System.Random as R
import qualified Control.Monad.State as S


-- http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
-- RANDOM NUMBERS
-- Ex. 8
bindRandom :: R.RandomGen g => (a -> g -> (b, g)) -> (g -> (a, g)) -> g -> (b, g)
bindRandom g h seed =
  let (x, seed') = h seed in g x seed'


-- Ex. 9
unitRandom  :: R.RandomGen g => a -> g -> (a, g)
unitRandom =
  (,)


addDigit :: (R.RandomGen g, S.MonadState g m) => Int -> m Int
addDigit x = do
  seed <- S.get
  let (y, seed') = R.randomR (0, 9) seed
  S.put seed'
  return (x + y)


randomSum :: (R.RandomGen g, S.MonadState g m) => Int -> m Int
randomSum =
  -- (*10) <$> addDigit z0 >>= addDigit
  -- ST.liftM (*10) (addDigit z0) >>= addDigit
  -- addDigit z0 >>= return . (*10) >>= addDigit
  addDigit >=> return . (*10) >=> addDigit


runRandomSumZero :: R.RandomGen g => g -> (Int, g)
runRandomSumZero =
  S.runState (randomSum 0)
