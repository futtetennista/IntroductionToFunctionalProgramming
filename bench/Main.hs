{-# LANGUAGE BangPatterns #-}

module Main (main)
where

import Weigh (Weigh, mainWith, func)
import Chapter5 (subs)
import Chapter7 (primes, fastprimes, hamming, hamming', fasthamming', genhamming, fastgenhamming)
import Data.List (foldl')


main :: IO ()
main =
  mainWith (do -- subsets
               -- foldsStrictF'
               -- foldsStrictF
               -- foldsNonStrictF
               -- primeNums
               hammingNums)


hammingNums :: Weigh ()
hammingNums =
  do func "first 10000 hamming nums (recursive)" (take 10000 . uncurry3 hamming') (2, 3, 5)
     func "first 10000 hamming nums (cyclic)" (take 10000 . uncurry3 fasthamming') (2, 3, 5)
     func "first 10000 hamming nums (general recursive)" (take 10000 . genhamming) [2, 3, 5, 7]
     func "first 10000 hamming nums (general cyclic)" (take 10000 . fastgenhamming) [2, 3, 5, 7]
       where
         uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
         uncurry3 f (x, y, z) =
           f x y z


primeNums :: Weigh ()
primeNums =
  do func "first 10000 primes (recursive sieve)" (takePrimes primes) 10000
     func "first 10000 primes (cyclic sieve)" (takePrimes fastprimes) 10000
       where
         takePrimes =
           flip take


foldsNonStrictF :: Weigh ()
foldsNonStrictF =
  do func "foldr (&&)" (foldr (&&) True) xs
     func "foldl (&&)" (foldl (&&) True) xs
     func "foldl' (&&)" (foldl' (&&) True) xs
     func "foldr (++)" (foldr (++) []) (copy 500 (copy 1000 'X'))
     func "foldl (++)" (foldl (++) []) (copy 500 (copy 1000 'X'))
     func "foldl' (++)" (foldl' (++) []) (copy 500 (copy 1000 'X'))
       where
         xs :: [Bool]
         xs =
           take 10 (repeat True) ++ take 1000 (repeat False)

         copy n x =
           [x | _ <- [1..n]]


foldsStrictF :: Weigh ()
foldsStrictF =
  do func "foldl' (+)" (calcSum foldl') xs
     func "foldl (+)" (calcSum foldl) xs
     func "foldr (+)" (calcSum foldr) xs
     func "foldl' (+) (wot?!)" (calcSum' foldl') 5000000
     func "foldl (+)  (wot?!)" (calcSum' foldl) 5000000
     func "foldr (+) (wot?!)" (calcSum' foldr) 5000000
       where
         xs :: [Int]
         xs =
           [1..5000000]

         -- calcSum' !f x =
         calcSum' f x =
           f (+) 0 xs
           where
             -- xs = ([1..x]::[Int])
             xs = seq const ([1..x]::[Int])

         calcSum f =
           f (+) 0


-- foldsStrictF' :: Weigh ()
-- foldsStrictF' =
--   do func "sum foldl'" (foldl' (+) 0) xs
--      func "sum foldl" (foldl (+) 0) xs
--      func "sum foldr" (foldr (+) 0) xs
--        where
--          xs :: [Int]
--          xs =
--            [1..5000000]


subsets :: Weigh ()
subsets =
  do func "subsets" subs ([1..15] :: [Int])
     func "subsets'" subs' ([1..15] :: [Int])
       where
         subs' :: [a] -> [[a]]
         subs' [] =
           [[]]
         subs' (x:xs) =
           yss ++ map (x:) yss
           where
             yss =
               subs' xs
