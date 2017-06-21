module TicTacToe

where

import qualified Data.List as L (intersperse, transpose)
import qualified Data.Char as C (isDigit)

type Grid =
  [[Player]]


putGrid :: Grid -> IO ()
putGrid =
  putStrLn . showGrid


--    |   |
--  O |   | X
--    |   |
-- -----------
--    |   |
--    |   |
--    |   |
-- -----------
--    |   |
--  X | X | O
--    |   |
showGrid :: Grid -> String
showGrid =
  unlines . concat . L.intersperse line . map showRow . indexed
  where
    indexed :: Grid -> [[(Int, Player)]]
    indexed =
      snd . foldr (\xs (i, acc) -> (i - 3, zip [i..] xs : acc)) (6, [])

    line :: [String]
    line =
      [replicate (size * 4 - 1) '-']

     --   |  |
     -- O |  | X = [O,B,X]
     --   |  |
    showRow :: [(Int, Player)] -> [String]
    showRow =
      beside . L.intersperse bar .  map showCell
      where
        bar :: [String]
        bar =
          replicate 3 "|"

       -- [[" x ","|"," x ", "|", " "], … ] => [" x | x |   "]
        beside :: [[[a]]] -> [[a]]
        beside =
          foldr1 (zipWith (++))

    showCell :: (Int, Player) -> [String]
    showCell x =
      [ "   "
      , content x
      , "   "
      ]
      where
        content (i, B) =
          " " ++ show i ++ " "
        content (_, p) =
          show p


empty :: Grid
empty =
  replicate size (replicate size B)


size :: Int
size =
  3


data Player
  = O
  | B
  | X
  deriving (Eq, Ord)


instance Show Player where
  show O =
    " O "
  show B =
    "   "
  show X =
    " X "


next :: Player -> Player
next O =
  X
next X =
  O
next B =
  B


-- Note: O starts first
turn :: Grid -> Player
turn g =
  if os <= xs then O else X
  where
    os =
      length $ filter (==O) flatGrid

    xs =
      length $ filter (==X) flatGrid

    flatGrid =
      concat g


move :: Grid -> Player -> Int -> [Grid]
move g p i =
  if valid g i
  then [chop size newFlatGrid]
  else []
  where
    newFlatGrid =
      take i flatGrid ++ [p] ++ drop (i + 1) flatGrid

    flatGrid =
      concat g

    chop :: Int -> [a] -> [[a]]
    chop _ [] =
      []
    chop n xs@(_:_) =
      take n xs : chop n (drop n xs)


valid :: Grid -> Int -> Bool
valid g i =
  i >= 0 && i < (size * size) && (B == concat g !! i)


win :: Grid -> Player -> Bool
win g p =
  any allPs (rows ++ cols ++ diags)
  where
    allPs =
      (all (==p))

    rows :: [[Player]]
    rows =
      g

    cols :: [[Player]]
    cols =
      L.transpose g

    diags :: [[Player]]
    diags =
      map (\xs -> [xs !! n !! n | n <- [0..(size - 1)]]) [g, map reverse g]


full :: Grid -> Bool
full =
  all notBlank
  where
    notBlank =
      all (/=B)


data State
  = Win Player
  | Draw
  | Playing Grid


ticTacToe :: IO ()
ticTacToe = do
  play empty (turn empty)
  where
    toIdx :: Grid -> Char -> [Int]
    toIdx g c =
      if C.isDigit c && valid g (read [c])
      then [read [c]]
      else []

    play g p = do
      putGrid g
      putStr ("It's '" ++ show p ++ "' turn: ")
      c <- getChar
      putChar '\n'
      case toIdx g c of
        [] ->
          do putStrLn $ show c ++ " is not a valid index" ; play g p

        [i] ->
          case play' g p i of
            Draw ->
              do putStrLn "It's a draw!" ; ticTacToe

            Win p' ->
              do putStrLn $ "'" ++ show p' ++ "' wins!" ; play empty (next p)

            Playing g' ->
              play g' (next p)

    play' g p i =
      case move g p i of
        [] ->
          Playing g

        [g'] ->
          gameState g'

    gameState g
      | win g O =
        Win O
      | win g X =
        Win X
      | full g =
        Draw
      | otherwise =
        Playing g
