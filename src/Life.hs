module Life (life, glider)
where

import qualified Data.Set as S (fromList, toList, foldr, union, empty)
import qualified Control.Concurrent as CC (threadDelay)


type Board =
  [Position]

type Position =
  (Int, Int)


glider :: Board
glider =
  [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]


-- http://comptechdoc.org/os/linux/howlinuxworks/linux_hlvt100.html
clearScreen :: IO ()
clearScreen =
  putStr "\ESC[2J"


writeAt :: Position -> String -> IO ()
writeAt p xs =
  do goto p ; putStr xs
  where
    goto (x, y) =
      putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"


width :: Int
width =
  10

height :: Int
height =
  10


putCells :: Board -> IO ()
putCells b =
  sequence_ [writeAt p "O" | p <- b]


isAlive :: Board -> Position -> Bool
isAlive =
  flip elem


isEmpty :: Board -> Position -> Bool
isEmpty b p =
  not (isAlive b p)


neighbours :: Position -> [Position]
neighbours (x, y) =
  map wrap [ (x - 1, y - 1)
           , (x, y - 1)
           , (x + 1, y - 1)
           , (x - 1, y)
           , (x + 1, y)
           , (x - 1, y + 1)
           , (x, y + 1)
           , (x + 1, y + 1)
           ]
  where
    wrap :: Position -> Position
    wrap (x', y') =
      (((x' - 1) `mod` width) + 1, ((y' - 1) `mod` height) + 1)


aliveNeighboursCount :: Board -> Position -> Int
aliveNeighboursCount b p =
  length [x | x <- neighbours p, isAlive b x]


survivors :: Board -> [Position]
survivors b =
  [p | p <- b, hasRequiredAliveNeighbours p]
  where
    hasRequiredAliveNeighbours p =
      aliveNeighboursCount b p `elem` [2, 3]


newBirths :: Board -> [Position]
newBirths b =
  [p | p <- nodups (concat . map neighbours $ b), isEmpty b p, hasExactlyThreeAliveNeighbours p]
  -- [p | p <- uniqueNeighbours, isEmpty b p, hasExactlyThreeAliveNeighbours p]
  where
    -- remove duplicates without respecting the order of the elems in the list
    nodups =
      flip nodupsHelp []

    nodupsHelp [] acc =
      acc
    nodupsHelp (x:xs) acc =
      nodupsHelp (filter (/=x) xs) (x : acc)

    uniqueNeighbours :: [Position]
    uniqueNeighbours =
      S.toList uns

    -- Time complexity: N + (N*M) ϵ O(N*M) where N = length b, M = cost of accNeighbours
    uns =
      S.foldr accNeighbours S.empty (S.fromList b)

    -- Time complexity: c + c*LOG(c) + (c + M) ϵ O(M) where c = 8 (# of neighbours), M = length accumulator
    accNeighbours =
      S.union . S.fromList . neighbours

    hasExactlyThreeAliveNeighbours p =
      aliveNeighboursCount b p == 3


nextGen :: Board -> Board
nextGen b =
  survivors b ++ newBirths b


life :: Board -> IO ()
life b = do
  clearScreen
  putCells b
  -- wait 500000
  waitSeconds 1
  life (nextGen b)
  where
    waitSeconds n =
       CC.threadDelay (n * 1000 * 1000)

  --   wait :: Int -> IO ()
  --   wait n =
  --     sequence_ [return () | _ <- [0..n]]
