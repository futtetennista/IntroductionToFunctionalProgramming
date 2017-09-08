module BloomFilter.Internal ( IBloom(..)
                            , MBloom(..)
                            )

where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed ((!), UArray, bounds)
import Data.Word (Word32)
import Control.DeepSeq (NFData, rnf)


data IBloom a =
  IB { ihash  :: (a -> [Word32])
     , iarray :: UArray Word32 Bool
     }


-- UHU ?! Is this really in normal form? Are the elements already in nf?!
-- instance NFData (IBloom a) where
--   rnf bfilt =
--     len bfilt `seq` ()
--     where
--       len =
--         succ . snd . bounds . iarray


instance (NFData a) => NFData (IBloom a) where
  rnf (IB _ arr) =
    let
      x =
        visit arr 0
    in
      x `seq` ()
    where
      visit arr n
        | n == snd (bounds arr) =
            ()
        | otherwise =
            let
              x =
                rnf (arr ! n)
              n' =
                succ n
            in
              x `seq` n' `seq` visit arr n'


data MBloom s a =
  MB { mhash :: (a -> [Word32])
     , marray :: STUArray s Word32 Bool
     }
