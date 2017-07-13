{-# LANGUAGE OverloadedStrings #-}
module Typeclasses
where

import Control.Applicative (many)
import Control.Monad.State ((>=>))
import qualified System.Random as R
import qualified Control.Monad.State as S
import qualified Data.Monoid as M
import qualified Calculator as P
import qualified Data.Text as T



mkBTree :: Bool -> T.Text -> BTree a
mkBTree bst =
  head . until singleton combine . trees
  where
    singleton :: [a] -> Bool
    singleton =
      undefined

    combine :: [BTree a] -> [BTree a]
    combine =
      undefined

    trees :: T.Text -> [BTree a]
    trees xs =
      case P.parse (many tree) xs of
        [(bts, "")] ->
          bts

        _ ->
          []

    tree :: P.Parser (BTree a)
    tree =
      undefined


--MONADS

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
  -- ST.liftM (*10) (addDigit z0) >>= addDigit
  -- addDigit z0 >>= return . (*10) >>= addDigit
  addDigit >=> return . (*10) >=> addDigit
  -- as Stephen Diehl puts it, `g >>= return . f` calls for applicative style. Not sure it's idiomatic to mix applicative and monadic style tough
  -- (*10) <$> addDigit z0 >>= addDigit


runRandomSumZero :: R.RandomGen g => g -> (Int, g)
runRandomSumZero =
  S.runState (randomSum 0)


-- MONOIDS
-- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
data BTree a
  = Leaf a
  | Node (BTree a) a (BTree a)
  | Empty
  deriving (Eq, Show)


sampleBTree :: BTree Int
sampleBTree =
  Node (Leaf 1) 7 (Leaf 2)


instance Functor BTree where
  fmap _ Empty =
    Empty
  fmap f (Leaf x) =
    Leaf (f x)
  fmap f (Node l x r) =
    Node (fmap f l) (f x) (fmap f r)


instance Foldable BTree where
  foldMap _ Empty =
    mempty
  foldMap f (Leaf x) =
    f x
  foldMap f (Node l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r


instance Traversable BTree where
  -- traverse :: (Applicative f) => (a -> f b) -> BTree a -> f (BTree b)
  traverse g (Leaf x) =
    undefined


-- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
maxElement :: BTree Int -> Maybe (MaxN Int)
maxElement =
  foldMap (mmaxN . check)
  where
    mmaxN Nothing =
      Nothing
    mmaxN (Just x) =
      Just (MaxN x)


minElement :: BTree Int -> Maybe (MinN Int)
minElement =
  foldMap (mminN . check)
  where
    mminN Nothing =
      Nothing
    mminN (Just x) =
      Just (MinN x)


-- btrees with values in the open range (minBound, maxBound)
minmaxElement :: BTree Int -> Maybe (MinN Int, MaxN Int)
minmaxElement =
  foldMap (mtuple . check)
  where
    mtuple Nothing =
      Nothing
    mtuple (Just x) =
      Just (MinN x, MaxN x)


check :: Int -> Maybe Int
check x
  | x == minBound =
    Nothing
  | x == maxBound =
    Nothing
  | otherwise =
    Just x


newtype MaxN a =
  MaxN { getMaxN :: a }
  deriving (Eq, Show)


instance (Bounded a, Ord a, Num a) => Monoid (MaxN a) where
  mempty =
    MaxN maxBound -- Not sure about this instance

  (MaxN x) `mappend` (MaxN y) =
    MaxN $ coerce (>=) x y


newtype MinN a =
  MinN { getMinN :: a }
  deriving (Eq, Show)


instance (Bounded a, Num a, Ord a) => Monoid (MinN a) where
  mempty =
    MinN minBound -- Not sure about this instance

  (MinN x) `mappend` (MinN y) =
    MinN $ coerce (<=) x y


coerce :: Ord a => (a -> a -> Bool) -> a -> a -> a
coerce binp x y
  | binp x y =
    x
  | otherwise =
    y


findNth :: Int -> BTree a -> Maybe a
findNth n =
  find' ((==n) . fst) . btree'
  where
    -- Party pooper i.e. for BTree Int
    find :: Monoid a => ((Int, a) -> Bool) -> BTree (Int, a) -> Maybe a
    find p =
      foldMap (\x -> if p x then Just (snd x) else Nothing)

    find' :: ((Int, a) -> Bool) -> BTree (Int, a) -> Maybe a
    find' _ Empty =
      Nothing
    find' p (Leaf x)
      | p x =
        Just (snd x)
      | otherwise =
        Nothing
    find' p (Node l x r)
      | maybe False (const True) (find' p l) =
        find' p l
      | p x =
        Just (snd x)
      | maybe False (const True) (find' p r) =
        find' p r
      | otherwise =
        Nothing

    btree' :: BTree a -> BTree (Int, a)
    btree' t =
      S.evalState (label t) 1

    label :: BTree a -> S.State Int (BTree (Int, a))
    label Empty =
      return Empty
    label (Leaf x) =
      Leaf <$> mkLabel x
    label (Node l x r) =
      Node <$> label l <*> mkLabel x <*> label r

    mkLabel x =
      (,) <$> getAndIncrement <*> pure x

    getAndIncrement =
      do i <- S.get ; S.put (i + 1) ; return i
