module Main where

import Chapter6


main :: IO ()
main =
  do putStrLn "foldr" ; print $ foldrAppend 50 100
     putStrLn "foldl" ; print $ foldlAppend 50 100
     putStrLn "foldr'" ; print $ foldl'Append 50 100
