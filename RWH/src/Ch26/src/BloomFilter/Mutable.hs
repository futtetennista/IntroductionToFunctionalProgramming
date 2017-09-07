module BloomFilter.Mutable ( MBloom(..)
                           , elem
                           , notElem
                           , insert
                           , length
                           , new
                           )
where


import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)
import BloomFilter.Internal (MBloom(..))


new ::(a -> [Word32]) -> Word32 -> ST s (MBloom s a)
new hashfunc numBits  =
  MB hashfunc <$> newArray (0, numBits - 1) False


elem, notElem :: a -> MBloom s a -> ST s Bool
elem x bfilt =
  indices bfilt x >>= allM (readArray (marray bfilt))
  where
    allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
    allM _ [] =
      return True
    allM f (y:ys) = do
      ok <- f y
      if ok then allM f ys else return False


notElem x bfilt =
  (return . not) =<< elem x bfilt


insert :: MBloom s a -> a -> ST s ()
insert bfilt x =
  indices bfilt x >>=
    mapM_ (\bit -> writeArray (marray bfilt) bit True)


indices :: MBloom s a -> a -> ST s [Word32]
indices bfilt x = do
  modulus <- length bfilt
  return . map (`mod` modulus) $ (mhash bfilt) x


length :: MBloom s a -> ST s Word32
length bfilt =
  (succ . snd) <$> getBounds (marray bfilt)
