module Ch26.BloomFilter ( Bloom
                        , elem
                        , notElem
                        , MBF.insert
                        , length
                        , MBF.new
                        , easyList
                        )
where


import Ch26.Internal.BloomFilter (Bloom(..))
import qualified Ch26.Mutable.BloomFilter as MBF (MBloom(..), insert, new)
import Ch26.Hash (Hashable(..), doubleHash)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)
import Data.List (genericLength)
import Data.Maybe (catMaybes)


length :: Bloom a -> Int
length =
  fromIntegral . len


len :: Bloom a -> Word32
len =
  succ . snd . bounds . array


elem :: a -> Bloom a -> Bool
elt `elem` bfilt =
  all test (hash bfilt elt)
  where
    test hashx =
      array bfilt ! (hashx `mod` len bfilt)


notElem :: a -> Bloom a -> Bool
elt `notElem` filt =
  not (elt `elem` filt)


fromList :: (a -> [Word32]) -- family of hash functions to use
         -> Word32          -- number of bits in filter
         -> [a]             -- values to populate with
         -> Bloom a
fromList hash' numBits values =
{-
Interesting! The following does not type-check: B hash' . runSTUArray $ do ...
but this does: B hash' $ runSTUArray $ do ...
Why is that? It's not apparent to me looking at the type signature of ($) and (.)
-}
  B hash' array'
  where
    array' =
      runSTUArray $ do
        mb  <- MBF.new hash' numBits
        mapM_ (MBF.insert mb) values
        return (MBF.marray mb)


-- The expected rate of false positives (between 0 and 1)
type ErrorRate =
  Double


easyList :: (Hashable a) => ErrorRate -> [a] -> Either String (Bloom a)
easyList errRate xs =
   case suggestSizing (genericLength xs) errRate of
     Left err ->
       Left err
     Right (bits, numHashes) ->
       Right bfilt
       where
         bfilt =
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
