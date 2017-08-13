module Main where

import Chapter6
import qualified RWH.Ch13.Passwdmap as P

main :: IO ()
main =
  P.main
  -- putStrLn "foldr" ; print $ foldrAppend 50 100
  -- putStrLn "foldl" ; print $ foldlAppend 50 100
  -- putStrLn "foldr'" ; print $ foldl'Append 50 100
