module Chapter9
where

import Prelude hiding (until, Left, Right)
import Chapter6 (qsortBy)


data BTree a
  = Tip a
  | Bin (BTree a) (BTree a)


instance Show a => Show (BTree a) where
  show (Tip a) =
    show a
  show (Bin left right) =
    "{" ++ show left ++ "." ++ show right ++ "}"


size :: BTree a -> Int
size (Tip _) =
  1
size (Bin left right) =
  size left + size right


nsize :: BTree a -> Int
nsize (Tip _) =
  0
nsize (Bin left right) =
  1 + size left + size right


depth :: BTree a -> Int
depth (Tip _) =
  0
depth (Bin left right) =
  1 + max (depth left) (depth right)


tips :: BTree a -> [a]
tips (Tip x) =
  [x]
tips (Bin left right) =
  tips left ++ tips right


-- Time complexity: S (maptree) + N (foldtree) = O(N) if the tree is balanced where S is the size of the tree and N is 3 * P where P is the size of a subtree (given the two subtrees have the same size)
-- TODO: come up with a better analysis
tips' :: BTree a -> [a]
tips' =
  foldtree (++) . maptree unit
  where
    unit x =
      [x]


-- Ex. 9.1.8
-- Time complexity: S (maptree) + S (foldtree) = O(S) where S is the size of the tree
bettertips :: BTree a -> [a]
bettertips t =
  mtips t []
  where
    -- Making sense of this function:
    -- :t (:) :: a -> [a] -> [a]
    -- :t (1:) :: Num a => [a] -> [a]
    -- :t ((1:).) :: Num a => (a1 -> [a]) -> a1 -> [a]
    -- :t (.) :: (b -> c) -> (a -> b) -> a -> c
    -- :t ((1:).(2:)) :: Num a => [a] -> [a]
    -- :t ((1:).(2:)) [] :: Num a => [a]
    mtips :: BTree a -> [a] -> [a]
    -- mtips tree xs = foldtree (.) (maptree (:) tree) xs
    mtips =
      foldtree (.) . maptree (:)


maptree :: (a -> b) -> BTree a -> BTree b
maptree f (Tip x) =
  Tip (f x)
maptree f (Bin left right) =
  Bin (maptree f left) (maptree f right)


foldtree :: (a -> a -> a) -> BTree a -> a
foldtree _f (Tip x) =
  x
foldtree f (Bin left right) =
  f (foldtree f left) (foldtree f right)


-- Ex. 9.1.6
minimalBTree :: [a] -> BTree a
minimalBTree (x:xs) =
  minimalBTreeHelp xs (Tip x)
  where
    minimalBTreeHelp [] tree =
      tree
    minimalBTreeHelp (z:zs) (Tip y) =
      minimalBTreeHelp zs (Bin (Tip y) (Tip z))
    minimalBTreeHelp (z:zs) tree@(Bin left right)
      | size left == size right =
        minimalBTreeHelp zs (Bin tree (Tip z))
      | otherwise =
        minimalBTreeHelp zs (Bin left (minimalBTreeHelp [z] right))


testMinimalBTree :: Eq a => [a] -> Bool
testMinimalBTree xs =
  xs == tips btree && minimalDepth btree
  where
    btree =
      minimalBTree xs

    minimalDepth :: BTree a -> Bool
    minimalDepth tree =
      depth tree == ceiling (logBase 2.0 (fromIntegral $ size tree))


-- Huffman coding trees
data Step
  = Left
  | Right
  deriving Show


type Path =
  [Step]


decodexs :: BTree Char -> Path -> String
decodexs t =
  trace t t
  where
    trace _ (Tip x) [] =
      [x]
    trace tree (Tip x) remPath@(_:_) =
      x : trace tree tree remPath
    trace tree (Bin left _) (Left:zs) =
      trace tree left zs
    trace tree (Bin _ right) (Right:zs) =
      trace tree right zs
    trace _ _ _ =
      error "Invalid code"


encodexs :: BTree Char -> String -> Path
encodexs htree =
  concat . map (encodex htree)
  where
    encodex :: BTree Char -> Char -> Path
    encodex t =
      head  . codesx t

    codesx (Tip x) c
      | x == c =
        [[]]
      | otherwise =
        []
    codesx (Bin left right) c =
      map (Left:) (codesx left c) ++ map (Right:) (codesx right c)


testHuffmanCoding :: IO ()
testHuffmanCoding =
  do let
       e = encodexs htree "text"
       d = decodexs htree e
     putStrLn "Original Text: \"text\""
     putStrLn $ "Encoded text: " ++ show e
     putStrLn $ "Decoded text: \"" ++ d ++ "\""
  where
    htree =
      betterbuild (zip ['x', 'e', 't'] [1, 1, 2])


slowbuild :: [(Char, Int)] -> BTree Char
slowbuild =
  maptree fst . head . until single combine . map Tip
  where
    combine :: [BTree (Char, Int)] -> [BTree (Char, Int)]
    combine (t1:t2:ts) =
      insert (Bin t1 t2) ts

    insert :: BTree (Char, Int) -> [BTree (Char, Int)] -> [BTree (Char, Int)]
    insert t [] =
      [t]
    insert t1 (t2:ts)
      | weight t1 <= weight t2 =
        t1 : t2 : ts
      | otherwise =
        t2 : insert t1 ts

    weight (Tip (_, w)) =
      w
    weight (Bin right left) =
      weight right + weight left


single :: [a] -> Bool
single xs =
  1 == length xs


until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
  | p x =
    x
  | otherwise =
    until p f (f x)


type Weight =
  Int

data HTree
  = Leaf Weight Char
  | Node Weight HTree HTree


betterbuild :: [(Char, Int)] -> BTree Char
betterbuild =
  unlabel . head . until single combine . map leaf
  where
    leaf (c, w) =
      Leaf w c

    unlabel (Leaf _ c) =
      Tip c
    unlabel (Node _ left right) =
      Bin (unlabel left) (unlabel right)

    combine (t1:t2:ts) =
      insert (Node nodeWeight t1 t2) ts where nodeWeight = weight t1 + weight t2

    insert htree [] =
      [htree]
    insert htree (t:ts)
      | weight htree <= weight t =
        htree : t : ts
      | otherwise =
        t : insert htree ts

    weight (Leaf w _) =
      w
    weight (Node w _ _) =
      w


testBuildHTree :: BTree Char
testBuildHTree =
  betterbuild (zip ['g', 'r', 'a', 't', 'e'] [8, 9, 11, 13, 17])


-- Ex. 9.2.7
data LabelledBTree a
  = LLeaf a
  | LNode a (LabelledBTree a) (LabelledBTree a)
  deriving Show


-- Time complexity:
--   O(D * S)                  [concatenating S codes of length D]
-- + O(L * S * (logBase 2 S))  [encoding L chars]
-- + O(S * (logBase 2 S))      [building the dict]
-- = O((D * S) + L * S * (logBase 2 S))
-- where D is the depth of the given htree and S its size and L is the length of the input string
encodexs' :: BTree Char -> String -> Path
encodexs' htree =
  concat . map encodex
  where
    encodex :: Char -> Path
    encodex =
      bsearch dict

    -- Time complexity: O(logBase 2 S) where S is the size of htree
    bsearch :: LabelledBTree (Char, Path) -> Char -> Path
    bsearch (LLeaf (x, path)) query
      | x == query =
        path
      | otherwise =
        error $ "Unknown char to encode '" ++ show query ++ "'"
    bsearch (LNode (x, path) left right) query
      | x == query =
        path
      | query > x =
        bsearch right query
      | query < x =
        bsearch left query

    -- Time complexity: O(S * (logBase 2 S)) where S is the size of htree
    dict :: LabelledBTree (Char, Path)
    dict =
      buildDict . qsortBy fst $ codes htree [] []

    -- Builds the code dictionary as a balanced bst
    -- Time complexity: O(S) where n is the size of the htree
    buildDict :: [(Char, Path)] -> LabelledBTree (Char, Path)
    buildDict [x] =
      LLeaf x
    buildDict xs@(_:_) =
      LNode (xs !! mid) (buildDict (take mid xs)) (buildDict (drop (mid + 1) xs))
      where
        mid =
          length xs `div` 2

    -- Time complexity: O(S) where n is the size of htree
    codes :: BTree Char -> Path -> [(Char, Path)] -> [(Char, Path)]
    codes (Tip x) xpath paths =
      (x, reverse xpath) : paths
    codes (Bin left right) xpath paths =
      codes right (Right:xpath) (codes left (Left:xpath) paths)


testHuffmanCoding' :: IO ()
testHuffmanCoding' =
  do putStrLn "Original Text: \"text\""
     putStrLn $ "Encoded text: " ++ show encoded
     putStrLn $ "Decoded text: \"" ++ decoded ++ "\""
  where
    encoded =
      encodexs' htree "text"

    decoded =
      decodexs htree encoded

    htree =
      betterbuild (zip ['x', 'e', 't'] [1, 1, 2])
