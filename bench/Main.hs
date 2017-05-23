module Main (main)
where

import Weigh (Weigh, mainWith, func)
import Chapter5 (subs)


main :: IO ()
main =
  mainWith $ do subsets


subsets :: Weigh ()
subsets =
  do func "subs" subs ([1..15] :: [Int])
     func "subs'" subs' ([1..15] :: [Int])


subs' :: [a] -> [[a]]
subs' [] =
  [[]]
subs' (x:xs) =
  yss ++ map (x:) yss
  where
    yss =
      subs' xs
