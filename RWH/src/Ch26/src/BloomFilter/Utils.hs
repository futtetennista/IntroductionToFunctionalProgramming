module BloomFilter.Utils ( easyList
                         , suggestSizing
                         , sizings
                         )
where

import Prelude hiding (elem, length, notElem)
import Data.List (genericLength)
import Data.Maybe (catMaybes)
import BloomFilter.Hash (Hashable(..), doubleHash)
import BloomFilter.Immutable (IBloom, fromList)
import Data.Word (Word32)


-- The expected rate of false positives (between 0 and 1)
type ErrorRate =
  Double


easyList :: Hashable a => ErrorRate -> [a] -> Either String (IBloom a)
easyList errRate xs =
  either Left (Right . bfilt) $ suggestSizing (genericLength xs) errRate
  where
    bfilt (bits, numHashes) =
      fromList (doubleHash numHashes) bits xs


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
