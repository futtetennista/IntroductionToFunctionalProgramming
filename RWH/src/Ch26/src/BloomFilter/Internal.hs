module BloomFilter.Internal ( IBloom(..)
                            , MBloom(..)
                            )

where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)


data IBloom a =
  IB { ihash  :: (a -> [Word32])
     , iarray :: UArray Word32 Bool
     }


data MBloom s a =
  MB { mhash :: (a -> [Word32])
     , marray :: STUArray s Word32 Bool
     }
