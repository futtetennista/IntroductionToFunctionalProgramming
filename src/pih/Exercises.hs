module Pih.Exercises

where

import System.IO (hSetEcho, stdin)
import Countdown
import TicTacToe
import System.Random (randomRIO)


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
      evalCount eval
    -- evalCount = sum ms

    ms :: [Int]
    ms =
      map sum nss

    nss :: [[Int]]
    nss =
      map (map length) evalsss

    evalsss :: [[[Int]]]
    evalsss =
      map (map eval) es


es :: [[Expr]]
es =
  map exprs (choices [1, 3, 7, 10, 25, 50])


type Evaluator =
  Expr -> [Int]


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

    eval' (Val x) =
      [x]
    eval' (App op x y) =
      [apply op x' y' | x' <- eval' x
                      , y' <- eval' y
                      , valid'' op x' y']

    valid'' Div x y =
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
    nodes node@(Node x ts) =
      node : concat (map nodes ts)

    nodeCount :: Int
    nodeCount =
      n where Node n _ = ncount gtree
      --length (nodes gtree)


ncount :: GTree a -> GTree Int
ncount (Node _ []) =
  Node 1 []
ncount (Node _ ts) =
  Node n ts'
  where
    ts' =
      map ncount ts

    n =
      1 + foldr (\(Node x _) acc -> acc + x) 0 ts'


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
  n <- randomRIO (0, length bestmoves)
  return (bestmoves !! n)
  where
    bestmoves =
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
    minDepth x@(Node (_, _, d) _) y@(Node (_, _, d') _)
      | d <= d' =
        x
      | otherwise =
        y


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
