{-# LANGUAGE OverloadedStrings #-}
module Pih.Chapter9

where

import Countdown


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
  if n == 4672540 then Right n else Left n
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
