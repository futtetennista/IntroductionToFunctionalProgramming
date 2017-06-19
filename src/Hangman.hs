module Hangman (hangman)
where


type TargetWord =
  String

hangman :: IO ()
hangman = do
  word <- getWord
  loop word "" 0
  where
    loop word guess mistakeCount =
      do c <- getChar
         let state =
               play word guess mistakeCount c
         case state of
           Lost (p, n) ->
             putState p n

           Won ->
             hangman

           Playing (p, g, n) ->
             do putState p n ; loop word g n

    putState xs n =
      do putStrLn $ " " ++ xs ; putStr (hangmanFigure !! n)

    getWord :: IO String
    getWord =
      do putStr "Enter a word: " ; getLine


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


type DashedString =
  String


data State
  = Lost (DashedString, Int)
  | Won
  | Playing (DashedString, String, Int)


play :: TargetWord -> String -> Int -> Char -> State
play word guess mistakeCount c =
  if gameOver newMistakeCount
  then Lost (progress, newMistakeCount)
  else if guessing progress
  then Playing (progress, newGuess, newMistakeCount)
  else Won
  where
    (progress, newGuess, success) =
      reveal word guess c

    newMistakeCount =
      if success then mistakeCount else mistakeCount + 1

    gameOver =
      (== length hangmanFigure - 1)

    guessing =
      elem '-'


reveal :: TargetWord -> String -> Char -> (DashedString, String, Bool)
reveal word guess c =
  (progress, newGuess, elem c word)
  where
    newGuess =
      c:guess

    progress =
      [dash x | x <- word]

    dash x
      | elem x newGuess =
          x
      | otherwise =
          '-'
