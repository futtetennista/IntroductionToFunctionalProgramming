module Chapter9
where

import Prelude hiding (until, Left, Right, lookup, curry, uncurry)
import Data.List (intercalate)
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
foldtree _ (Tip x) =
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


data LBTree a b
  = LTip a
  | LBin b (LBTree a b) (LBTree a b)


instance (Show a, Show b) => Show (LBTree a b) where
  show (LTip x) =
    show x
  show (LBin y t1 t2) =
    "{" ++ show t1 ++ "." ++ "#" ++ show y  ++ "." ++ show t2 ++ "}"


tipslbtree :: LBTree a b -> [a]
tipslbtree lbtree =
  tipslbtreeHelp lbtree []
  where
    tipslbtreeHelp :: LBTree a b -> [a] -> [a]
    tipslbtreeHelp =
      foldlbtree (.) . maplbtree (:)


sizelbtree :: LBTree a b -> Int
sizelbtree (LTip _) =
  1
sizelbtree (LBin _ t1 t2) =
  sizelbtree t1 + sizelbtree t2


maplbtree :: (a -> c) -> LBTree a b -> LBTree c b
maplbtree f (LTip x) =
  LTip (f x)
maplbtree f (LBin y t1 t2) =
  LBin y (maplbtree f t1) (maplbtree f t2)


foldlbtree :: (a -> a -> a) -> LBTree a b -> a
foldlbtree _ (LTip x) =
  x
foldlbtree f (LBin _ t1 t2) =
  f (foldlbtree f t1) (foldlbtree f t2)


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


data BSTree a
  = Nil
  | Bin' a (BSTree a) (BSTree a)


instance Show a => Show (BSTree a) where
  show Nil =
    ""
  show (Bin' x left right) =
    "{" ++ show left ++ "." ++ show x ++ "." ++ show right ++ "}"


mapbstree :: (a -> b) -> BSTree a -> BSTree b
mapbstree _ Nil =
  Nil
mapbstree f (Bin' x left right) =
  Bin' (f x) (mapbstree f left) (mapbstree f right)


foldbstree :: (a -> a -> a) -> a -> BSTree a -> a
foldbstree _ z0 Nil =
  z0
foldbstree f z0 (Bin' x left right) =
  foldbstree f (f x (foldbstree f z0 left)) right -- in-order
  -- foldbstree f right (foldbstree f left (f x z0)) -- pre-order
  -- f x (f (foldbstree f left z0) (foldbstree f right z0)) -- post-order


depth' :: BSTree a -> Int
depth' Nil =
  0
depth' (Bin' _ left right) =
  1 + max (depth' left) (depth' right)


depthbal :: BSTree a -> Bool
depthbal Nil =
  True
depthbal (Bin' _ left right) =
  abs(depth' left - depth' right) <= 1
  && depthbal left
  && depthbal right


rebal :: BSTree a -> BSTree a
rebal Nil =
  Nil
rebal tree@(Bin' _ _ _)
  | slope tree == 2 =
    shiftr tree
  | slope tree == -2 =
    shiftl tree
  | otherwise =
    tree
  where
    slope (Bin' _ left right) =
      depth' left - depth' right

    shiftr (Bin' x t1 t2)
      | slope t1 == -1 =
        rotr(Bin' x (rotl t1) t2)
      | otherwise =
        rotr (Bin' x t1 t2)

    shiftl (Bin' x t1 t2)
      | slope t2 == 1 =
        rotl(Bin' x t1 (rotr t2))
      | otherwise =
        rotl (Bin' x t1 t2)

    rotr (Bin' x (Bin' y t1 t2) t3) =
      Bin' y t1 (Bin' x t2 t3)

    rotl (Bin' x t1 (Bin' y t2 t3)) =
      Bin' y (Bin' x t1 t2) t3


-- Set as bstree (conditionally balanced)
type Set' a =
  BSTree a


type Rebalance =
  Bool


insert :: Ord a => a -> Rebalance -> Set' a -> Set' a
insert x _ Nil =
  Bin' x Nil Nil
insert x b (Bin' y t1 t2)
  | x > y =
    condRebal $ Bin' y t1 (insert x b t2)
  | otherwise =
    condRebal $ Bin' y (insert x b t1) t2
  where
    condRebal tree =
      if b then rebal tree else tree


-- Ex. 9.4.3
delete :: Ord a => a -> Rebalance -> Set' a -> Set' a
delete _ _ Nil =
  Nil
delete x b (Bin' y left right)
  | x > y =
    condRebal $ Bin' y left (delete x b right)
  | x < y =
    condRebal $ Bin' y (delete x b left) right
  | x == y =
    condRebal $ join left right
  where
    condRebal tree =
      if b then rebal tree else tree

    join Nil t2 =
      t2
    join t1 t2 =
      Bin' z t t2
      where
        (z, t) =
          split t1


split :: BSTree a -> (a, BSTree a)
split (Bin' x t1 Nil) =
  (x, t1)
split (Bin' x t1 t2) =
  (y, Bin' x t1 t)
  where
    (y, t) =
      split t2


type Depths =
  (Int, Int)


-- Set as balanced bstree with cached depths
type Set'' a =
  BSTree (a, Depths)


-- Ex. 9.4.2
fastrebal :: BSTree (a, Depths) -> BSTree (a, Depths)
fastrebal Nil =
  Nil
fastrebal tree@(Bin' (_, (dleft, dright)) _ _)
  | slope == 2 =
    shiftr tree
  | slope == -2 =
    shiftl tree
  | otherwise =
    tree
  where
    slope =
      dleft - dright

    shiftr (Bin' x t1@(Bin' (_, (dt3, dt4)) _t3 _t4) t2)
      | dt3 - dt4 == -1 =
        rotr (Bin' x (rotl t1) t2)
      | otherwise =
        rotr (Bin' x t1 t2)

    shiftl (Bin' x t1 t2@(Bin' (_, (dt3, dt4)) _t3 _t4))
      | dt3 - dt4 == 1 =
        rotl (Bin' x t1 (rotr t2))
      | otherwise =
        rotl (Bin' x t1 t2)

    rotr (Bin' (x, (_, dt3)) (Bin' (y, (dt1, dt2)) t1 t2) t3) =
      Bin' (y, (dt1, fastdepth newT2)) t1 newT2
      where
        newT2 =
          Bin' (x, (dt2, dt3)) t2 t3

    rotl (Bin' (x, (dt1, _)) t1 (Bin' (y, (dt2, dt3)) t2 t3)) =
      Bin' (y, (fastdepth newT1, dt3)) newT1 t3
      where
        newT1 =
          Bin' (x, (dt1, dt2)) t1 t2


fastdepth :: BSTree (a, Depths) -> Int
fastdepth Nil =
  0
fastdepth (Bin' (_, (dt1, dt2)) _t1 _t2) =
  1 + max dt1 dt2


fastinsert :: Ord a => a -> Set'' a -> Set'' a
fastinsert x Nil =
  Bin' (x, (0, 0)) Nil Nil
fastinsert x tree@(Bin' (y, (dt1, dt2)) t1 t2)
  | x > y =
    fastrebal $ Bin' (y, (dt1, fastdepth newT2)) t1 newT2
  | x < y =
    fastrebal $ Bin' (y, (fastdepth newT1, dt2)) newT1 t2
  | otherwise =
    tree
  where
    newT1 =
      fastinsert x t1

    newT2 =
      fastinsert x t2


fastdelete :: Ord a => a -> Set'' a -> Set'' a
fastdelete _ Nil =
  Nil
fastdelete x (Bin' (y, (dt1, dt2)) t1 t2)
  | x > y =
    Bin' (y, (dt1, depth' newT2)) t1 newT2
  | x < y =
    Bin' (y, (depth' newT1, dt2)) newT1 t2
  | x == y =
    join t1 t2
  where
    newT1 =
      fastdelete x t1

    newT2 =
      fastdelete x t2

    join Nil t2' =
      t2'
    join t1' t2'@(Bin' (_, (dl, dr)) _ _) =
      Bin' (z, (depth' t, max dl dr)) t t2'
      where
        ((z, _), t) =
          split t1'


-- Set as ordered list
type Set a =
  [a]


add :: Ord a => a -> Set a -> Set a
add x [] =
  [x]
add x xs@(_:_)
  | x > midX =
    take (mid + 1) xs ++ add x (drop (mid + 1) xs)
  | x < midX =
    add x (take mid xs) ++ drop mid xs
  | otherwise =
    xs
  where
    midX =
      xs !! mid
    mid =
      length xs `div` 2


remove :: Ord a => a -> Set a -> Set a
remove _ [] =
  []
remove x xs@(_:_)
  | x > midX =
    take (mid + 1) xs ++ remove x (drop (mid + 1) xs)
  | x < midX =
    remove x (take mid xs) ++ drop mid xs
  | x == midX =
    take mid xs ++ drop (mid + 1) xs
  where
    midX =
      xs !! mid

    mid =
      length xs `div` 2


-- Ex. 9.4.5
-- ƛ: sumSetBalBSTree' 10000   (0.11 secs,   79,920,752 bytes)
-- ƛ: sumSetOrderedList 10000  (3.47 secs,    5,666,114,808 bytes)
-- ƛ: sumSetBSTree 10000       (8.98 secs,   11,679,908,808 bytes)
-- ƛ: sumSetBalBSTree 10000    (126.56 secs, 56,911,774,856 bytes)
sumSetOrderedList :: Int -> Integer
sumSetOrderedList n =
  foldr (+) 0 . map toInteger $ foldr add [] xs
  where
    xs =
      drop (n `div` 2) [1..n] ++ take (n `div` 2) [1..n]


sumSetBSTree :: Int -> Integer
sumSetBSTree n =
  foldbstree (+) 0 . mapbstree toInteger $ foldr (flip insert False) Nil xs
  where
    xs =
      drop (n `div` 2) [1..n] ++ take (n `div` 2) [1..n]


sumSetBalBSTree :: Int -> Integer
sumSetBalBSTree n =
  foldbstree (+) 0 . mapbstree toInteger $ foldr (flip insert True) Nil xs
  where
    xs =
      drop (n `div` 2) [1..n] ++ take (n `div` 2) [1..n]


sumSetBalBSTree' :: Int -> Integer
sumSetBalBSTree' n =
  foldbstree (+) 0 . mapbstree (toInteger . fst) $ foldr fastinsert Nil xs
  where
    xs =
      drop (n `div` 2) [1..n] ++ take (n `div` 2) [1..n]


sizebal :: LBTree a b -> Bool
sizebal (LTip _) =
  True
sizebal (LBin _ t1 t2) =
  abs(sizelbtree t1 - sizelbtree t2) <= 1
  && sizebal t1
  && sizebal t2


type Array a =
  LBTree a Int


type Index =
  Int


-- Builds non-empty arrays
mkarray :: [a] -> Array a
mkarray xs
  | n == 1 =
    LTip (head xs)
  | n > 1 =
    LBin sizeT1 (mkarray ys) (mkarray zs)
  where
    n =
      length xs

    sizeT1 =
      n `div` 2

    ys =
      take sizeT1 xs

    zs =
      drop sizeT1 xs


-- Ex. 9.5.4
mkarray2 :: [a] -> Array a
mkarray2 xs =
  fst (mkarray2' (length xs) xs)
  where
    mkarray2' n xs'
      | n == 1 =
        (LTip (head xs'), tail xs')
      | n > 1 =
        (LBin sizeT1 t1 t2, zs)
      where
        sizeT1 =
          n `div` 2

        (t1, ys) =
          mkarray2' sizeT1 xs'

        (t2, zs) =
          mkarray2' (n - sizeT1) ys


-- lookup :: Array a -> Index -> Maybe a
lookup :: Array a -> Index -> a
lookup (LTip x) k
  | k == 0 =
    x
  | otherwise =
    undefined
lookup (LBin sizeT1 t1 t2) k
  | k < sizeT1 =
    lookup t1 k
  | otherwise =
    lookup t2 (k - sizeT1)


-- update :: Array a -> Index -> a -> Maybe (Array a)
update :: Array a -> Index -> a -> Array a
update (LTip _) k x
  | k == 0 =
    LTip x
  | otherwise =
    undefined
update (LBin sizeT1 t1 t2) k x
  | k < sizeT1 =
    LBin sizeT1 (update t1 k x) t2
  | otherwise =
    LBin sizeT1 t1 (update t2 (k - sizeT1) x)


length' :: Array a -> Int
length' (LTip _) =
  1
length' (LBin sizeT1 _ t2) =
  sizeT1 + length' t2


-- General Trees
data GTree a =
  GNode a [GTree a]
  deriving Eq


instance Show a => Show (GTree a) where
  show (GNode x []) =
    show x
  show (GNode x ts) =
    show x ++ ".{" ++ intercalate "," (map show ts) ++ "}"


sizegtree :: GTree a -> Int
sizegtree (GNode _ ts) =
  1 + sum (map sizegtree ts)


depthgtree :: GTree a -> Int
depthgtree (GNode _ ts)
  | length ts == 0 =
    0
  | otherwise =
    1 + maximum (map depthgtree ts)


data ExpTree a
  = ExpTip a
  | ExpBin (ExpTree a) (ExpTree a)


instance Show a => Show (ExpTree a) where
  show (ExpTip x) =
    show x
  show (ExpBin t1 t2) =
    "{" ++ show t1 ++ "." ++ show t2  ++ "}"


curry :: GTree a -> ExpTree a
curry (GNode x ts) =
  foldl ExpBin (ExpTip x) (map curry ts)


-- f (g x y) u (h w)
expressionGTree :: GTree String
expressionGTree = GNode "f" [ GNode "g" [ GNode "x" [], GNode "y" [] ]
                            , GNode "u" []
                            , GNode "h" [ GNode "w" [] ]
                            ]


showExprgtree :: GTree String -> String
showExprgtree (GNode x []) =
  x
showExprgtree (GNode x gts@(_:_)) =
  "(" ++ x ++ foldr (\x acc -> " " ++ showExprgtree x ++ acc) [] gts ++ ")"
  -- "(" ++ x ++ foldr ((++) . (" "++) . showExprgtree) [] gts ++ ")"


uncurry :: ExpTree a -> GTree a
uncurry (ExpTip x) =
  GNode x []
uncurry (ExpBin t1 t2) =
  GNode x (ts ++ [t])
  where
    GNode x ts =
      uncurry t1

    t =
      uncurry t2


-- Ex. 9.6.1
mapgtree :: (a -> b) -> GTree a -> GTree b
mapgtree f (GNode x gts) =
  GNode (f x) (map (mapgtree f) gts)


-- Ex. 9.6.3
foldlgtree :: (b -> a -> b) -> b -> GTree a -> b
foldlgtree f z0 (GNode x gts) =
  foldl (foldlgtree f) (f z0 x) gts

-- wrongfoldlgtree f z0 (GNode x []) = f z0 x
-- wrongfoldlgtree f z0 (GNode x (gt:gts)) = wrongfoldlgtree f (foldl (wrongfoldlgtree f) (f z0 x) gts) gt
