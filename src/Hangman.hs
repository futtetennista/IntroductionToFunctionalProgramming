module Hangman
where

import Control.Monad (when, unless)
-- import Control.Monad.Trans.State.Strict (StateT, put, get, modify)


-- type State = StateT (Input, Guess) IO

type Input =
  String

type Guess =
  String

hangman :: IO ()
hangman =
  do
    putStr "Enter a word: "
    word <- getLine
    play word ""
    hangman
    where
      play word guess =
        do c <- getChar
           let (progress, newGuess) =
                 game word guess c
           putStrLn $ " " ++ progress
           when (guessing progress) (play word newGuess)
             where
               guessing xs =
                 xs `contains` '-'


game :: String -> String -> Char -> (String, String)
game word guess c =
  (reveal, (c:guess))
  where
    reveal =
      [dash x | x <- word]

    dash x
      | (c:guess) `contains` x =
          x
      | otherwise =
          '-'


contains :: String -> Char -> Bool
contains xs c  =
  [] /= filter (==c) xs
