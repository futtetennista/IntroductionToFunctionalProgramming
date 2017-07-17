{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RWH.Ch15.SupplyClass ( S.Supply
                            , S.runSupply
                            , S.runRandomSupply
                            , MonadSupply (..)
                            )
where

import qualified RWH.Ch15.Supply as S

class (Monad m) => (MonadSupply s) m | m -> s where
  next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
  next = S.next


showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class =
  showTwo <$> next <*> next
  where
    showTwo x y =
      (show "a: " ++ show x ++ ", b: " ++ show y)
