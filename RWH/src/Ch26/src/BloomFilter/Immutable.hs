module BloomFilter.Immutable ( IBloom
                             , elem
                             , notElem
                             , MBF.insert
                             , length
                             , MBF.new
                             , fromList
                             )
where


import BloomFilter.Internal (IBloom(..))
import qualified BloomFilter.Mutable as MBF (MBloom(..), insert, new)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)


length :: IBloom a -> Int
length =
  fromIntegral . len


len :: IBloom a -> Word32
len =
  succ . snd . bounds . iarray


elem :: a -> IBloom a -> Bool
x `elem` bfilt =
  all test (ihash bfilt x)
  where
    test hashx =
      iarray bfilt ! (hashx `mod` len bfilt)


notElem :: a -> IBloom a -> Bool
x `notElem` filt =
  not (x `elem` filt)


fromList :: (a -> [Word32]) -- family of hash functions to use
         -> Word32          -- number of bits in filter
         -> [a]             -- values to populate with
         -> IBloom a
fromList hash' numBits values =
{-
Interesting! The following does not type-check: B hash' . runSTUArray $ do ...
but this does: B hash' $ runSTUArray $ do ...
Why is that? It's not apparent to me looking at the type signature of ($) and (.)
-}
  IB hash' array'
  where
    array' =
      runSTUArray $ do
        mb  <- MBF.new hash' numBits
        mapM_ (MBF.insert mb) values
        return (MBF.marray mb)
