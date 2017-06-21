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


-- it feels odd to have 'B' as a player
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


play :: State -> Player -> IO ()
play Draw _ =
  do putStrLn "It's a draw!" ; tictactoe
play (Win p') p =
  do putStrLn $ "'" ++ show p' ++ "' wins!" ; play (Playing empty) (next p)
play (Playing g) p = do
  putGrid g
  g' <- play' p
  play (gameState g') (next p)
  where
    play' :: Player -> IO Grid
    play' X = do
      return $ bestmove 9 g X
    play' O = do
      putStr ("It's '" ++ show O ++ "' turn: ")
      c <- getChar
      putChar '\n'
      case toIdx g c of
        [] ->
          do putStrLn $ show c ++ " is not a valid index" ; return g

        [i] ->
          return (head . move g O $ i)

    toIdx :: Grid -> Char -> [Int]
    toIdx g' c =
      if C.isDigit c && valid g' (read [c])
      then [read [c]]
      else []

    gameState :: Grid -> State
    gameState g'
      | win g' O =
        Win O
      | win g' X =
        Win X
      | full g' =
        Draw
      | otherwise =
        Playing g'


tictactoe :: IO ()
tictactoe = do
  putStrLn "Which player should start: 'X' or 'O' ?"
  c <- getChar
  case toPlayer c of
    Nothing ->
      do putStrLn "Invalid player" ; tictactoe

    Just p ->
      play (Playing empty) p
  where
    toPlayer c
      | c `elem` "xX" =
        Just X
      | c `elem` "oO" =
        Just O
      | otherwise =
        Nothing


data GTree a =
  Node a [GTree a]
  deriving (Show, Eq)


moves :: Grid -> Player -> [Grid]
moves g p
  | win g p =
    []
  | full g =
    []
  | otherwise =
    concat [move g p i | i <- [0..(size * size) - 1]]


gametree :: Grid -> Player -> GTree Grid
gametree g p =
  Node g [gametree g' (next p) | g' <- moves g p]


prune :: Int -> GTree a -> GTree a
prune 0 (Node x _) =
  Node x []
prune n (Node x ts) =
  Node x [prune (n - 1) t | t <- ts]


minimax :: GTree Grid -> GTree (Grid, Player)
minimax (Node g [])
  | win g O =
    Node (g, O) []
  | win g X =
    Node (g, X) []
  | otherwise =
    Node (g, B) []
minimax (Node g ts)
  | turn g == O =
    Node (g, minimum ps) ts'
  | turn g == X =
    Node (g, maximum ps) ts'
  where
    ts' =
      map minimax ts

    ps =
      [p | Node (_, p) _ <- ts']


type Estimate =
  Int


minimax' :: GTree Grid -> GTree (Grid, Estimate)
minimax' =
  undefined


bestmove :: Int -> Grid -> Player -> Grid
bestmove n g p =
  head [g' | Node (g', p') _ <- ts, p' == best]
  where
    gtree =
      prune n (gametree g p)

    Node (_, best) ts =
      minimax gtree
