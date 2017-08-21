module Hangman (hangman)
where

import System.IO (hSetEcho, stdin)

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
             do putState p (Just n) ; putStrLn ("You lost! The word was: " ++ word) ; hangman

           Won ->
             do putState word Nothing ; putStrLn "Got it!" ; hangman

           Playing (p, g, n) ->
             do putState p (Just n) ; loop word g n

    putState xs mn =
      do putStrLn $ " " ++ xs ; maybe (return ()) (putStr . (hangmanFigure !!)) mn


getWord :: IO String
getWord = do
  putStr "Enter a word: " ; sgetLine'
  where
    sgetChar =
      do hSetEcho stdin False ; c <- getChar ; hSetEcho stdin True ; return c

    -- sgetLine' = do
    --   c <- sgetChar
    --   if c == '\n' then do putChar '\n' ; return [] else do putChar '*' ; cs <- sgetLine' ; return (c:cs)

    sgetLine' :: IO String
    sgetLine' =
      getLineHelp []
      where
        getLineHelp xs = do
          c <- sgetChar
          case c of
            '\DEL' ->
              do putStr "\b\ESC[0K" ; getLineHelp (safeTail xs)
            '\n' ->
              do putChar '\n' ; return (reverse xs)
            _ ->
              do putChar '*' ; getLineHelp (c:xs)

        safeTail [] =
          []
        safeTail (_:xs) =
          xs


    -- sgetLine input = do
    --   c <- sgetChar
    --   if c == '\n'
    --   then do putChar '\n' ; return (reverse input)
    --   else do putChar '*' ; sgetLine (c : input)


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
