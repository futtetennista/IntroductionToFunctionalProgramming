{-# LANGUAGE OverloadedStrings #-}
module Typeclasses
where

import Control.Applicative ((<|>), many, some)
import Control.Monad.State ((>=>))
import qualified Data.Foldable as F
import qualified System.Random as R
import qualified Control.Monad.State as S
import qualified Data.Monoid as M
import qualified Calculator as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.List as L


mkBTree' :: Bool -> [Int] -> BTree Int
mkBTree' bst =
  mkBTree bst . text
  -- foldr T.append T.empty . L.intersperse " " . fmap (T.pack . show)
  where
    text =
      TL.toStrict . TLB.toLazyText . foldr buildText mempty

    buildText x builder =
      builder `mappend` (TLB.fromString $ show x)


-- TODO: use smth more performant than lists
mkBTree :: Bool -> T.Text -> BTree Int
mkBTree bst =
  combineTrees . fmap Leaf . condOrd . trees
  where
    condOrd =
      if bst then qsort else id

    qsort :: Ord a => [a] -> [a]
    qsort [] =
      []
    qsort (x:xs) =
      qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

    combineTrees :: [BTree Int] -> BTree Int
    combineTrees xs
      | not bst =
        L.foldl' mkBTree Empty xs
      | otherwise =
        mkBSTree xs
      where
        mkBSTree [] =
          Empty
        mkBSTree ys@(_:_) =
          case (mkBSTree lts, mkBSTree rts) of
            (Empty, Empty) ->
              midt

            (lt, rt) ->
              Node lt midx rt
          where
            size =
              length ys

            midt@(Leaf midx) =
              ys !! (size `div` 2)

            (lts, rts) =
              case splitAt (size `div` 2) ys of
                (lts', (_:rts')) ->
                  (lts', rts')

                x ->
                  x

        mkBTree Empty t =
          t
        mkBTree t1@(Leaf _) (Leaf x) =
          Node t1 x Empty
        mkBTree (Node t1 x Empty) t2 =
          Node t1 x t2
        mkBTree t1 (Leaf x) =
          Node t1 x Empty

    trees :: T.Text -> [Int]
    trees xs =
      case P.parse (many $ P.natural) xs of
        [(bts, "")] ->
          bts

        [(_, ts)] ->
          error $ "Invalid input: " ++ T.unpack ts ++ ". Valid input example: \"1 2 3 4\""

        _ ->
          []


bstree :: Ord a => BTree a -> Bool
bstree (Node lt x rt) =
  all (==True) [ comparelabel (>=) x lt
               , comparelabel (<) x rt
               , bstree lt
               , bstree rt
               ]
  where
    comparelabel f y (Node _ z _ ) =
      f y z
    comparelabel f y (Leaf z) =
      f y z
bstree _ =
  True


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
  = Empty
  | Leaf a
  | Node (BTree a) a (BTree a)
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


-- a must be Foldable, that's the key to understand why [] can be more useful than Maybe when accumulating results
instance Foldable BTree where
  foldMap _ Empty =
    mempty
  foldMap f (Leaf x) =
    f x
  foldMap f (Node l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r


instance Traversable BTree where
  -- traverse :: (Applicative f) => (a -> f b) -> BTree a -> f (BTree b)


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


-- 1. (MaxN minBound) `max` x = x
-- 2. x `max` (MaxN minBound) = x
-- 3. (x `max` y) `max` z = x `max` (y `max` z)
instance (Bounded a, Ord a, Num a) => Monoid (MaxN a) where
  mempty =
    MaxN minBound

  (MaxN x) `mappend` (MaxN y) =
    MaxN (max x y)


newtype MinN a =
  MinN { getMinN :: a }
  deriving (Eq, Show)


-- 1. (MinN maxBound) `min` x = x
-- 2. x `min` (MinN maxBound) = x
-- 3. (x `min` y) `min` z = x `min` (y `min` z)
instance (Bounded a, Num a, Ord a) => Monoid (MinN a) where
  mempty =
    MinN maxBound

  (MinN x) `mappend` (MinN y) =
    MinN (min x y)


findNth :: Int -> BTree a -> [a]
findNth n btree =
  S.evalState (mfind'' btree) 1
  -- case S.runState (mfind' btree) 1 of
  --   Right _ ->
  --     []
  --   Left x ->
  --     [x]
  -- find'' ((==n) . fst) (btree' btree)
  where
    -- Finding stuff this way doesn't seem to be a good fit for monads since computation stops when a "failure" is encountered.
    mfind :: BTree a -> S.State Int (Maybe a)
    mfind Empty =
      return Nothing
    mfind (Leaf x) = do
      i <- S.get
      if i == n then return $ Just x else do S.put (i + 1) ; return Nothing
    mfind (Node l x r) = do
      ml <- mfind l
      maybe (do i <- S.get
                if i == n then return $ Just x else do S.put (i + 1) ; mfind r)
        (return . Just) ml

    -- This actually works but it's kind of a hack since the Either type is used semantically in the opposite way wrt conventions. The worst thing it's that mfind' is a partial function, this is even worst considering it returns smth of type Either a b.
    mfind' :: BTree a -> S.State Int (Either a a)
    mfind' Empty =
      undefined -- well…
    mfind' (Leaf x) = do
      i <- S.get
      if i == n then return (Left x) else do S.put (i + 1) ; return (Right x)
    mfind' (Node l x r) = do
      my <- mfind' l
      case my of
        Right _ ->
          do i <- S.get
             if i == n then return (Left x) else do S.put (i + 1) ; mfind' r

        res ->
          return res

    -- Best solution so far: no hacky use of predefined types, no need to build and traverse a 2nd tree and being lazy evaluation stops whenever a (the) result is found! Keys: leverage foldable and lists to represent success.
    mfind'' :: BTree a -> S.State Int [a]
    mfind'' =
      F.foldlM find []
      where
        find :: [a] -> a -> S.State Int [a]
        find [x] _ =
          return [x]
        find [] x = do
          i <- S.get
          if i == n then return [x] else S.put (i + 1) >> return []

    -- Applicative style doesn't seem a good fit for this problem either because the computation isn't fixed (think the iffy function in "Applicative programming with effects").
    afind :: BTree a -> S.State Int [a]
    afind =
      undefined

    -- First try `find'' :: ((Int, a) -> Bool) -> BTree (Int, a) -> Maybe a` was a party pooper cause of the monoid constraint (i.e. for BTree Int) if the return type is Maybe. Solution: use [] instead of Maybe!
    -- Massively inefficient since the whole tree must be traversed twice
    find'' :: ((Int, a) -> Bool) -> BTree (Int, a) -> [a]
    find'' p =
      foldMap (\x -> if p x then [snd x] else [])

    find :: ((Int, a) -> Bool) -> BTree (Int, a) -> [a]
    find _ Empty =
      []
    find p (Leaf x)
      | p x =
        [snd x]
      | otherwise =
        []
    find p (Node l x r)
      | length (find p l) == 1 =
        find p l
      | p x =
        [snd x]
      | length (find p r) == 1 =
        find p r
      | otherwise =
        []

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


-- MONADPLUS
-- TMR Issue 11: MonadPlus: What a Super Monad!
splits :: Eq a => [a] -> [(a, [a])]
splits xs =
  xs >>= \x -> return (x, L.delete x xs)


-- memoize the choosen number so that successive choices won't contain it using monad trans…cool!
choose :: Eq a => S.StateT [a] [] a
choose =
  S.StateT splits


sendmoney' :: S.StateT [Int] [] [(Char, Int)]
sendmoney' = do
  s <- choose
  S.guard (s > 7)
  e <- choose ; n <- choose ; d <- choose ; r <- choose ; y <- choose
  S.guard $ num [s, e, n, d] + num [m, o, r, e] == num [m, o, n, e, y]
  return $ zip "sendmoremoney" [s, e, n, d, m, o, r, e, m, o, n, e, y]
  where
    (m, o) =
      (1, 0)

    num =
      foldl ((+) . (*10)) 0
