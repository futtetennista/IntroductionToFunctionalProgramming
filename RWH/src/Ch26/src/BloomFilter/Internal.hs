module BloomFilter.Internal ( IBloom(..)
                            , MBloom(..)
                            )

where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed ((!), UArray, bounds)
import Data.Word (Word32)
import Control.DeepSeq

data IBloom a =
  IB { ihash  :: (a -> [Word32])
     , iarray :: UArray Word32 Bool
     }

instance (NFData a) => NFData (IBloom a) where
  rnf (IB _ arr) =
    let n = visit arr in n `seq` ()
    where
      visit arr n
        | n == 0 =
            ()
        | otherwise =
            let
              x = rnf (arr ! n)
              n' = n + 1
            in
              n' `seq` visit arr n'


data MBloom s a =
  MB { mhash :: (a -> [Word32])
     , marray :: STUArray s Word32 Bool
     }
