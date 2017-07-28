{-# LANGUAGE OverloadedStrings #-}
module Pih.Exercises

where

import System.IO (hSetEcho, stdin)
import qualified Countdown as CD
import qualified Calculator as Calc
import TicTacToe hiding (State, next)
import qualified System.Random as R
import Control.Monad.State (StateT, State)
import Control.Applicative ((<$>), many)
import qualified Data.Text as T (Text)
import qualified Control.Monad.State as S

default (T.Text)


-- Ex. 9.2
-- isChoice [1, 2] [1, 3, 2] == True; isChoice [2, 3, 5] [1, 3, 5] == False
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ =
  True
-- isChoice (x:xs) ys = elem x ys && isChoice xs ys -- O(N * M) where N = length xs, M = length ys
isChoice (x:xs) ys =
  -- length (foldr (defenestrate' []) ys xs) == length ys - length xs
  -- elem x ys && isChoice xs (defenestrate' [] x ys)
  -- fst $ foldr (\x acc -> (elem x (snd acc) && fst acc, defenestrate x (snd acc))) (True, ys) xs
  isChoiceHelp (x:xs) ys True
  where
    isChoiceHelp _ _ False =
      False
    isChoiceHelp [] _ True =
      True
    isChoiceHelp (z:zs) ws True =
      isChoiceHelp zs (minusElem z ws) (elem z ws)

    -- Time complexity: O(Z) where Z = length (z:zs)
    minusElem :: Eq a => a -> [a] -> [a]
    minusElem x ys =
      minusElemHelp x ys []
      where
        minusElemHelp _ [] acc =
          acc
        minusElemHelp y (z:zs) acc
          | y == z =
            acc ++ zs
          | otherwise =
            minusElemHelp y zs (acc ++ [z])


-- Ex. 9.4
verifyExprCount :: Either Int Int
verifyExprCount =
  wrap n
  where
    wrap =
      if n == 33665406 then Right else Left

    n =
      sum (map length es)


verifyEvalCount :: Either Int Int
verifyEvalCount =
  if n == 4672540
  then Right n
  else Left n
  where
    n =
      evalCount CD.eval
    -- evalCount = sum ms

    ms :: [Int]
    ms =
      map sum nss

    nss :: [[Int]]
    nss =
      map (map length) evalsss

    evalsss :: [[[Int]]]
    evalsss =
      map (map CD.eval) es


es :: [[CD.Expr]]
es =
  map CD.exprs (CD.choices [1, 3, 7, 10, 25, 50])


type Evaluator =
  CD.Expr -> [Int]


evalCount :: Evaluator -> Int
evalCount f =
  sum [1 | evRess <- map (map f) es
         , evRes <- evRess
         , _valid <- evRes]


-- Ex. 9.5
verifyEvalCount' :: Either Int Int
verifyEvalCount' =
  if n == 10839369
  then Right n
  else Left n
  where
    n =
      evalCount eval'

    eval' (CD.Val x) =
      [x]
    eval' (CD.App op x y) =
      [CD.apply op x' y' | x' <- eval' x
                         , y' <- eval' y
                         , valid'' op x' y']

    valid'' CD.Div x y =
      y /= 0 && x `mod` y == 0
    valid'' _ _ _ =
      True


-- Ex. 10.1
putStr' :: String -> IO ()
putStr' xs =
  sequence_ [putChar x | x <- xs]


-- Ex. 10.5
adder :: IO ()
adder = do
  putStrLn "How Many nums?"
  n <- readInt
  xs <- sequence $ take (toInt n) (repeat readInt)
  putStrLn $ "Total:" ++ show (sum . map toInt $ xs)
  where
    readInt =
      do c <- getChar ; putChar '\n' ; return c

    singleton x =
      [x]

    toInt :: Char -> Int
    toInt x =
      read (singleton x)


-- Ex. 10.6
getLine' :: IO String
getLine' =
  getLineHelp []
  where
    getLineHelp xs = do
      c <- getCh
      case c of
        '\DEL' ->
          do putStr "\b\ESC[0K" ; getLineHelp (safeTail xs)
        '\n' ->
          do putChar '\n' ; return (reverse xs)
        _ ->
          do putChar c ; getLineHelp (c:xs)

    getCh = do
      hSetEcho stdin False ; c <- getChar ; hSetEcho stdin True ; return c

    safeTail [] =
      []
    safeTail (_:xs) =
      xs


-- Ex. 11.1
verifyGameTreeCount :: Either String Int
verifyGameTreeCount =
  depthE >> nodeCountE
  where
    depthE =
      if d == 9 then Right d else Left ("depth=" ++ show d) where d = depth gtree

    nodeCountE =
      if nodeCount == 549946 then Right nodeCount else Left ("#nodes=" ++ show nodeCount)

    gtree =
      gametree empty X

    nodes :: GTree a -> [GTree a]
    nodes node@(Node _ ts) =
      node : concat (map nodes ts)

    nodeCount :: Int
    nodeCount =
      label (ncount gtree)
      --length (nodes gtree)


label :: GTree a -> a
label (Node x _) =
  x


ncount :: GTree a -> GTree Int
ncount (Node _ []) =
  Node 1 []
ncount (Node _ ts) =
  Node n ts'
  where
    ts' =
      map ncount ts

    n =
      1 + foldr ((+) . label) 0 ts'


depth :: GTree a -> Int
depth (Node _ []) =
  0
depth (Node _ ts) =
  1 + maximum [depth t | t <- ts]


-- Fixed: https://ghc.haskell.org/trac/ghc/ticket/13106
-- ƛ: mapbtree (\Node _ ts -> length ts) (Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 []], Node 7 []])
-- ghc: panic! (the 'impossible' happened)
--   (GHC version 8.0.2 for x86_64-apple-darwin):
-- 	initTc: unsolved constraints
--   WC {wc_insol =
--         [W] mapbtree_aAty :: t_aAtx[tau:1] (CHoleCan: mapbtree)
--         [W] mapbtree_aAua :: t_aAu9[tau:1] (CHoleCan: mapbtree)}

-- Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug


-- Ex. 11.2
bestmoveRandom :: Int -> Grid -> Player -> IO Grid
bestmoveRandom n g p = do
  x <- R.randomRIO (0, length bestmvs)
  return (bestmvs !! x)
  where
    bestmvs =
      [g' | Node (g', p') _ <- ts', p' == best]

    Node (_, best) ts' =
      minimax . prune n . gametree g $ p


mapgtree :: (a -> b) -> GTree a -> GTree b
mapgtree f (Node x ts) =
  Node (f x) [mapgtree f t | t <- ts]


-- Ex. 11.3
bestmoveQuickWin :: Int -> Grid -> Player -> Grid
bestmoveQuickWin n g p =
  grid . mingtree . map gtreeDepth . bestmoves n g $ p
  where
    grid :: GTree (Grid, Player, Int) -> Grid
    grid (Node (g', _, _) _) =
      g'

    mingtree :: [GTree (Grid, Player, Int)] -> GTree (Grid, Player, Int)
    mingtree xs =
      foldr1 minDepth xs

    minDepth :: GTree (Grid, Player, Int) -> GTree (Grid, Player, Int) -> GTree (Grid, Player, Int)
    minDepth gx gy
      | d <= d' =
        gx
      | otherwise =
        gy
      where
        (_, _, d) =
          label gx

        (_, _, d') =
          label gy


gtreeDepth :: GTree (a, b) -> GTree (a, b, Int)
gtreeDepth (Node (g, p) ts) =
  Node (g, p, d) ts'
  where
    ts' =
      [gtreeDepth t | t <- ts]

    d =
      if null ts' then 0 else 1 + maximum (map nodeDepth ts')

    nodeDepth (Node (_, _, d') _) =
      d'


bestmoves :: Int -> Grid -> Player -> [GTree (Grid, Player)]
bestmoves n g p =
  [node | node@(Node (_, p') _) <- ts, p' == best]
  where
  Node (_, best) ts =
    minimax . prune n . gametree g $ p


data BTree a
  = Leaf a
  | Bin (BTree a) (BTree a)
  deriving Show


index :: a -> State Int (a, Int)
index x = do
  n <- S.get ; S.put (n + 1) ; return (x, n)


alabel :: BTree a -> State Int (BTree (a, Int))
alabel (Leaf x) =
  Leaf <$> index x
alabel (Bin l r) =
  Bin <$> alabel l <*> alabel r


mlabel :: BTree a -> State Int (BTree (a, Int))
mlabel (Leaf x) = do
  x' <- index x ; return (Leaf x')
mlabel (Bin l r) = do
  l' <- mlabel l ; r' <- mlabel r ; return (Bin l' r')


-- Ex. 11.1
-- Labelled binary tree
data LBTree a
  = Leaf'
  | Node' (LBTree a) a (LBTree a)
  deriving Show


instance Functor LBTree where
  fmap _ (Leaf') =
    Leaf'

  fmap f (Node' l x r) =
    Node' (fmap f l) (f x) (fmap f r)


-- Ex. 11.2
type Func a =
  (->) a

-- instance Functor Func where
  -- fmap :: (a -> b) -> f a -> f b
  -- f a :: ((->) t) t1
  -- fmap mf (f a) :: (t -> t2) -> (((->) t) t1) -> ((-> t1) t2)
  -- fmap mf (f a) :: (t -> t2) -> (t -> t1) -> (t -> t2) = (t1 -> t2) -> (t -> t1) -> t -> t2
fmap' :: Func b c -> Func a b -> a -> c
fmap' f g =
  f . g -- fmap = (.)


-- Ex. 11.3
-- GOTCHA 1: f is ((->) a) !!! Do this substitution everywhere `f` appears
-- GOTCHA 2: the importance of naming things !!! The `a` in `a -> f a` is not the same `a` as the one in `((->) a)`. Renaming one of two avoids confusion, i.e. pure :: t -> f t, (<*>) :: f (t1 -> t2) -> f t1 -> f t2
-- GOTCHA 3: use this renaming when declaring instances for all types with more than one param
-- GOTCHA 4: follow the types !!! Most of the times implementing an instance is trivial once you understand its type
-- instance Applicative ((->) a) where
--   -- pure :: t -> f t :: t -> (((->) a) t) :: t -> (a -> t) :: t -> a -> t
pure' :: Func a c -> b -> Func a c
pure' =
  const
--
--   -- (<*>) :: f (t1 -> t2) -> f t1 -> f t2
--   -- (<*>) :: (((->) a) -> (t1 -> t2)) -> (((->) a) t1) -> (((->) a) t2) :: (a -> t1 -> t2) -> (a -> t1) -> (a -> t2) :: (a -> t1 -> t2) -> (a -> t1) -> a -> t2
ap' :: (Func a (Func b c)) -> (Func a b) -> a -> c
ap' g h x =
  g x (h x) -- or: g <*> h = \x -> g x (h x)


-- Ex. 11.6
-- instance Monad ((->) a) where
--   -- (>>=) :: m t1 -> (t1 -> m t2) -> m t2 :: (((->) a) t1) -> (t1 -> (((->) a) t2)) -> (((->) a) t2) :: (a -> t1) -> (t1 -> a -> t2) -> (a -> t2) :: :: (a -> t1) -> (t1 -> a -> t2) -> a -> t2
--   (>>=) g h x = h (g x) x -- or: g >>= h = \x -> h (g x) x
bind' :: Func a b -> (Func b (Func a c)) -> Func a c
bind' g h =
  \x -> h (g x) x


bindList :: [a] -> (a -> [b]) -> [b]
bindList xs g =
  [y | x <- xs, y <- g x] -- concat [g x | x <- xs]

bindList' :: [a] -> (a -> [b]) -> [b]
bindList' xs f =
  concat (fmap f xs)


bindm :: Maybe a -> (a -> Maybe b) -> Maybe b
bindm Nothing _ =
  Nothing
bindm (Just x) g =
  g x

bindm' :: Maybe a -> (a -> Maybe b) -> Maybe b
bindm' m g =
  joinm (fmap g m)
  where
    joinm Nothing = Nothing
    joinm (Just mx) = mx


-- Ex. 11.4
newtype ZipList a =
  Z [a] deriving (Eq, Show)


instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f (Z xs) = Z (map f xs)


instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  -- Uhu?! How is pure x = Z [x] violating `fmap g x = pure g <*> x ?
  -- Aha! fmap (*2) (Z [1,2,3]) /= pure (*2) <*> (Z [1,2,3]) ~~> Z [2,4,6] /= Z [2]

  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) =
    -- Z $ zipWith ($) gs xs
    Z [g x | (g, x) <- zip gs xs]
    -- Z (fmap (\(g, x) -> g x) (zip gs xs))
    -- Z (go gs xs [])
    -- where
    --   go [] xs acc =
    --     reverse acc
    --   go (g:gs) (x:xs) acc =
    --     go gs xs (g x : acc)


-- ZipList isn't a monad:
-- 1. [x] return x >>= g == g x
--    return 1 >>= \x -> Z [x*2] < ~~> _|_ > /= (\x -> Z [x*2]) 1 < ~~> [2] >
-- 2. [?] m >>= return == m
-- 3. [?] (m >>= g) >>= h == m >>= (\x -> g x >>= h)
-- instance Monad ZipList where
  -- (>>=) :: ZipList a -> (a -> ZipList b) -> ZipList b
  -- Z xs >>= g =
    -- Uhu ?! why sequence [Z [1,2], Z [3,4]] == Z [[1,3],[2,4]] ?
    -- let Z yss = sequence [g x | x <- xs] in Z (concat yss)


-- Ex. 11.7
data Expr' a
  = Var' a
  | Val' Int
  | Add' (Expr' a) (Expr' a)
  deriving Show


instance Functor Expr' where
  -- fmap :: (a -> b) -> a -> b :: (a -> b) -> Expr' a -> Expr' b
  fmap f (Var' x) =
    Var' (f x)
  fmap _ (Val' x) =
    Val' x
  fmap f (Add' l r) =
    Add' (fmap f l) (fmap f r)


-- Ex: add prefix to all vars, i.e. ("x_"++) <$> (Add' (Var' "foo") (Add' (Val' 1) (Var' "bar")))
--     map vars to a tuple name, name length, i.e. (\x -> (x, length x)) <$> Add' (Add' (Var' "x") (Var' "foobar")) (Val' 1)
instance Applicative Expr' where
  -- pure :: a -> f a :: a -> Expr' a
  pure x =
    Var' x

  -- (<*>) :: f (a -> b) -> f a -> f b :: Expr' (a -> b) -> Expr' a -> Expr' b
  Var' g <*> Var' x =
    Var' (g x)
  Var' _ <*> Val' x =
    Val' x
  Val' x <*> _ =
    Val' x
  -- which order is the correct one ?! Is there a correct one or does it depend on the specs ?!
  g <*> Add' l r =
    Add' (g <*> l) (g <*> r)
  Add' lg rg <*> x =
    Add' (lg <*> x) (rg <*> x)


-- 3rd monad law:
-- (Add' (Val' 1) (Var' "x") >>= (return . ("f_"++))) >>= (return . ("g_"++))
-- Add' (Val' 1) (Var' "x") >>= (\x -> (return . ("f_"++)) x >>= (return . ("g_"++)))
instance Monad Expr' where
  -- (>>=) :: m a -> (a -> m b) -> m b :: Expr' a -> (a -> Expr' b) -> Expr' b
  Var' x >>= g =
    g x
  Val' x >>= _ =
    Val' x
  Add' l r >>= g =
    Add' (l >>= g) (r >>= g)


-- Ex. 11.8
type State' =
  Int


newtype ST a =
  S { app :: State' -> (a, State') }


instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st =
    -- do newSt <- st >>= (return . g) ; return newSt
    do s <- st ; return (g s) -- st >>= (\s -> return (g s)) -- st >>= (return . g)
    -- S (\s -> let (x, st') = app st s in (g x, st'))


instance Applicative ST where
  -- pure :: a -> ST a
  pure x =
    S (\s -> (x,s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    stf >>= \f -> stx >>= return . f
    -- do f <- stf ; x <- stx ; return (f x)
    -- stf >>= (\f -> stx >>= (\x -> return (f x)))
    -- S (\s -> let (f, st') = app stf s ; (x, st'') = app stx st' in (f x, st''))


instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f =
    S (\s -> let (x, s') = app st s in app (f x) s')


-- Ex. 11.1
parsecomm :: Calc.Parser ()
parsecomm = do
  _ <- sequence_ (replicate 2 (Calc.symbol "-")) -- or better: _ <- string "--"
  comm
  where
    comm =
      do _ <- many (Calc.mfilterp (/='\n')) ; _ <- Calc.char '\n' ; return ()


sampleComment :: T.Text
sampleComment =
  "-- Parse comment\nfoo :: Int"


-- 14.1
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
type Tuple a b =
  (,) a b

mempty'' :: (Monoid a, Monoid b) => Tuple a b
mempty'' =
  (mempty, mempty)

mappend'' :: (Monoid a, Monoid b) => Tuple a b -> Tuple a b -> Tuple a b
(x1, y1) `mappend''` (x2, y2) =
  (x1 `mappend` x2, y1 `mappend` y2)


-- 14.2
-- instance (Monoid b) => Monoid ((->) a b) where
mempty' :: Monoid b => Func a b
mempty' =
  const mempty

mappend' :: Monoid b => Func a b -> Func a b -> Func a b
mappend' f g =
  \x -> f x `mappend` g x


-- 14.4
instance Foldable LBTree where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> LBTree a -> m
  foldMap _ Leaf' =
    mempty
  foldMap f (Node' l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r


instance Traversable LBTree where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> LBTree a -> f (LBTree b)
  traverse _ Leaf' =
    pure Leaf'
  traverse g (Node' l x r) =
    Node' <$> traverse g l <*> g x <*> traverse g r


-- 14.5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p =
  foldMap (\x -> if p x then [x] else [])


-- 15.4
-- ƛ: Chapter7.fasterfib 500000
-- (21.66 secs, 11,241,527,416 bytes)
-- ƛ: head . drop 500000 $ lazyfibs
-- (8.61 secs, 11,204,292,392 bytes)
-- Why is this SO MUCH quicker than using cyclic structures ?!
-- Might it be because it doesn't have to traverse the list until the n-th position to get the value needed ? ==> Nope that doesn't seem plausible
lazyfibs :: [Integer]
lazyfibs =
  0 : 1 : [curr + next | (curr, next) <- zip lazyfibs (tail lazyfibs)]


fib :: Int -> Integer
fib n =
  head (drop n lazyfibs)


-- 15.5
data BTree' a
  = Nil
  | Bin' (BTree' a) a (BTree' a)


instance Show a => Show (BTree' a) where
  show Nil =
    ""
  show (Bin' l x r) =
    "{" ++ show l ++ "." ++ show x ++ "." ++ show r ++ "}"


repeatBTree' :: a -> BTree' a
repeatBTree' x =
  Bin' (repeatBTree' x) x (repeatBTree' x)


takeBTree' :: Int -> BTree' a -> BTree' a
takeBTree' _ Nil =
  Nil
takeBTree' 0 _ =
  Nil
takeBTree' n (Bin' l x r) =
  Bin' (takeBTree' (n - 1) l) x (takeBTree' (n - 1) r)


replicateBTree' :: Int -> a -> BTree' a
replicateBTree' n =
  takeBTree' n . repeatBTree'
