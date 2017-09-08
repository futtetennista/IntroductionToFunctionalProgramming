{-# LANGUAGE BangPatterns #-}
module BloomFilter.BloomFilter ( Bloom
                               , mkFromList
                               , mkFromList'
                               , suggestSizing
                               , sizings
                               , elem
                               , length
                               )
where

import Prelude hiding (elem, length, notElem)
import Data.List (genericLength)
import Data.Maybe (catMaybes)
import BloomFilter.Hash (Hashable(..), doubleHash)
import qualified BloomFilter.Immutable as B (IBloom(..), fromList, elem, length)
import Data.Word (Word32)


type Bloom =
  B.IBloom


-- The expected rate of false positives (between 0 and 1)
type ErrorRate =
  Double


elem :: a -> B.IBloom a -> Bool
elem =
  B.elem


length :: B.IBloom a -> Int
length =
  B.length


-- To build the Bloom filter strictly: let !ebf = mkFromList' 0.01 ([1..10^6]::[Int])
mkFromList' :: Hashable a => ErrorRate -> [a] -> Either String (B.IBloom a)
mkFromList' errRate xs =
  either Left rightBFilt' $ suggestSizing (genericLength xs) errRate
  where
    rightBFilt' x =
      let
        !bfilt =
          mkBFilt' x
      in
        Right bfilt

    mkBFilt' (bits, numHashes) =
      let
        bfilt@(B.IB _ !arr) =
          B.fromList (doubleHash numHashes) bits xs
      in
        bfilt


-- This only evaluates up to `suggestSizing`: let !ebf = mkFromList 0.01 ([1..10^6]::[Int])
mkFromList :: Hashable a => ErrorRate -> [a] -> Either String (B.IBloom a)
mkFromList errRate xs =
  either Left (Right . mkBFilt) $ suggestSizing (genericLength xs) errRate
  where
    mkBFilt (bits, numHashes) =
      B.fromList (doubleHash numHashes) bits xs


-- expected maximum capacity
type MaxCapacity =
  Integer


suggestSizing :: MaxCapacity
              -> ErrorRate
              -> Either String (Word32, Int) -- (filter size, number of hashes)
suggestSizing capacity errRate
  | capacity <= 0 =
      Left "capacity too small"
  | errRate <= 0 || errRate >= 1 =
      Left "invalid error rate"
  | null saneSizes =
      Left "capacity too large"
  | otherwise =
    Right (minimum saneSizes)
  where
    saneSizes =
      catMaybes . map sanitize $ sizings capacity errRate

    sanitize (bits, hashes)
      | bits > maxWord32 - 1 =
          Nothing
      | otherwise =
          Just (ceiling bits, truncate hashes)
      where
        maxWord32 =
          fromIntegral (maxBound :: Word32)


sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
  [(((-k) * cap / log (1 - (errRate ** (1 / k)))), k) | k <- [1..50]]
  where
    cap =
      fromIntegral capacity
