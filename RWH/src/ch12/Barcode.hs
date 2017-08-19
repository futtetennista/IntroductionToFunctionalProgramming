module RWH.Ch12.Barcode
where

import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt, intToDigit)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M


checkDigit :: (Integral a) => [a] -> a
checkDigit ds =
  10 - (sum products `mod` 10)
  where
    products =
      mapEveryOther (*3) (reverse ds)


mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f =
  zipWith ($) (cycle [f,id])


leftOddList :: [L.ByteString]
leftOddList =
  [ "0001101", "0011001", "0010011", "0111101", "0100011"
  , "0110001", "0101111", "0111011", "0110111", "0001011" ]


rightList :: [L.ByteString]
rightList =
  L.map complement <$> leftOddList
  where
    complement '0' = '1'
    complement '1' = '0'


leftEvenList :: [L.ByteString]
leftEvenList =
  map L.reverse rightList


parityList :: [L.ByteString]
parityList =
  [ "111111", "110100", "110010", "110001", "101100"
  , "100110", "100011", "101010", "101001", "100101" ]


listToArray :: [a] -> Array Int a
listToArray xs =
  listArray (0, l - 1) xs
  where
    l =
      length xs


leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int L.ByteString
leftOddCodes =
  listToArray leftOddList

leftEvenCodes =
  listToArray leftEvenList

rightCodes =
  listToArray rightList

parityCodes =
  listToArray parityList


-- | Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a =
  go s (indices a)
  where
    go s (j:js) =
      let s' = f s (a ! j)
      in s' `seq` go s' js
    go s _ = s


-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a =
  foldA f (a ! fst (bounds a)) a


tuple4 :: (a, a, a, a) -> Int -> Maybe a
tuple4 (x, y, w, z) idx
  | idx == 0 =
    Just x
  | idx == 1 =
    Just y
  | idx == 2 =
    Just w
  | idx == 3 =
    Just z
  | otherwise =
    Nothing


tuple6 :: (a, a, a, a, a, a) -> Int -> Maybe a
tuple6 (x, y, k, u, w, z) idx
  | idx == 0 =
    Just x
  | idx == 1 =
    Just y
  | idx == 2 =
    Just k
  | idx == 3 =
    Just u
  | idx == 4 =
    Just w
  | idx == 5 =
    Just z
  | otherwise =
    Nothing


tuple6' :: (a, a, a, a, a, a) -> Int -> Maybe a
tuple6' (x, y, k, u, w, z) idx
  | idx < 4 =
    tuple4 (x, y, k, u) idx
  | idx == 4 =
    Just w
  | idx == 5 =
    Just z
  | otherwise =
    Nothing


encodeEAN13 :: L.ByteString -> L.ByteString
encodeEAN13 =
  L.concat . encodeDigits . L.foldr ((:) . digitToInt) []


-- | This function computes the check digit; don't pass one in.
encodeDigits :: [Int] -> [L.ByteString]
encodeDigits s@(first:rest) =
  outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where
    (left, right) =
      splitAt 5 rest

    lefties =
      L.zipWith leftEncode (parityCodes ! first) $ (L.pack . show) left

    righties :: [L.ByteString]
    righties =
      map rightEncode (right ++ [checkDigit s])


leftEncode :: Char -> Char -> L.ByteString
leftEncode '1' idx =
  (leftOddCodes !) (digitToInt idx)
leftEncode '0' idx =
  (leftEvenCodes !) (digitToInt idx)


rightEncode :: Int -> L.ByteString
rightEncode =
  (rightCodes !)


outerGuard :: L.ByteString
outerGuard =
  "101"


centerGuard :: L.ByteString
centerGuard =
  "01010"
