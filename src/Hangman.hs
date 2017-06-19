module Hangman (hangman)
where

import Control.Monad (when)

hangman :: IO ()
hangman =
  do
    word <- getWord
    win <- play word "" 0
    when win hangman
    where
      getWord :: IO String
      getWord =
        do putStr "Enter a word: "
           getLine


play :: String -> String -> Int -> IO Bool
play word guess mistakeCount =
  do c <- getChar
     let (progress, newGuess, success) =
           game word guess c
         newMistakeCount =
           if success then mistakeCount else mistakeCount + 1
     putStrLn $ " " ++ progress
     putStr (hangmanFigure !! newMistakeCount)
     if gameOver newMistakeCount then return False
     else if guessing progress then play word newGuess newMistakeCount
     else return True
       where
         gameOver n =
           n == length hangmanFigure - 1

         guessing =
           elem '-'


--   O
--  /|\
--   |
--  / \
hangmanFigure :: [String]
hangmanFigure =
  [ ""
  , " O\n"
  , " O\n/\n"
  , " O\n/|\n"
  , " O\n/|\\\n"
  , " O\n/|\\\n |\n"
  , " O\n/|\\\n |\n/\n"
  , " O\n/|\\\n |\n/ \\\n"
  ]


game :: String -> String -> Char -> (String, String, Bool)
game word guess c =
  (reveal, newGuess, elem c word)
  where
    newGuess =
      c:guess

    reveal =
      [dash x | x <- word]

    dash x
      | elem x newGuess =
          x
      | otherwise =
          '-'
