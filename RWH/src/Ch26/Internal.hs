{-# LANGUAGE ExistentialQuantification #-}
module Ch26.Internal ( Bloom(..)
                     , MBloom(..)
                     )

where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)


data Bloom a =
  B { hash  :: (a -> [Word32])
    , array :: UArray Word32 Bool
    }


data MBloom s a =
  MB { mhash :: (a -> [Word32])
     , marray :: STUArray s Word32 Bool
     }
