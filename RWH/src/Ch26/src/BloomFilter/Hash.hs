{-# LANGUAGE ForeignFunctionInterface #-}
module BloomFilter.Hash ( Hashable(..)
                        , hash
                        , doubleHash
                        )
where

import Data.Bits ((.&.), shiftR)
import Foreign.Marshal.Array (withArrayLen)
import Control.Monad (foldM)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (words)


foreign import ccall unsafe "lookup3.h hashword2" hashWord2
  :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
  :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()


hashIO :: Ptr a   -- value to hash
       -> CSize   -- number of bytes
       -> Word64  -- salt
       -> IO Word64
hashIO ptr bytes salt =
  with (fromIntegral salt) $ \saltp -> do
    let
      salt1p =
        castPtr saltp
      salt2p =
        castPtr saltp `plusPtr` 4
    go salt1p salt2p
    peek saltp
    where
      go p1 p2
        | isByte =
            hashWord2 (castPtr ptr) words p1 p2
        | otherwise =
            hashLittle2 ptr bytes p1 p2
        where
          isByte =
            bytes .&. 3 == 0

      words =
        bytes `div` 4


class Hashable a where
  hashSalt :: Salt -> a -> Word64


type Salt =
  Word64


hash :: Hashable a => a -> Word64
hash =
  hashSalt 0x106fc397cf62f64d3


hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k =
  unsafePerformIO . with k $ \ptr ->
    hashIO ptr (fromIntegral (sizeOf k)) salt


instance Hashable Char where
  hashSalt =
    hashStorable


instance Hashable Int where
  hashSalt =
    hashStorable


instance Hashable Double where
  hashSalt =
    hashStorable


hashList :: (Storable a) => Word64 -> [a] -> IO Word64
hashList salt xs =
  withArrayLen xs $ \len ptr ->
    hashIO ptr (fromIntegral (len * sizex)) salt
  where
    sizex
      | null xs =
        0
      | otherwise =
        sizeOf (head xs)


instance (Storable a) => Hashable [a] where
  hashSalt salt xs =
    unsafePerformIO $ hashList salt xs


hash2 :: (Hashable a) => a -> Word64 -> Word64
hash2 k salt =
  hashSalt salt k


instance (Hashable a, Hashable b) => Hashable (a, b) where
  hashSalt salt (a, b) =
    hash2 b . hash2 a $ salt


instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
    hashSalt salt (a, b, c) =
      hash2 c . hash2 b . hash2 a $ salt


hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs =
  Strict.useAsCStringLen bs $ \(ptr, len) ->
    hashIO ptr (fromIntegral len) salt


instance Hashable Strict.ByteString where
  hashSalt salt bs =
    unsafePerformIO $ hashByteString salt bs


-- Ensures that the chunks we pass to the C hashing code are a uniform 64KB in size,
-- so that we will give consistent hash values no matter where the original chunk boundaries lie.
rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
  | Lazy.null s =
      []
  | otherwise =
      let
        (pre, suf) =
          Lazy.splitAt chunkSize s
      in
        repack pre : rechunk suf
    where
      repack =
        Strict.concat . Lazy.toChunks

      chunkSize =
        64 * 1024


instance Hashable Lazy.ByteString where
  hashSalt salt bs =
    unsafePerformIO $ foldM hashByteString salt (rechunk bs)


doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value =
  [h1 + h2 * i | i <- [0..num]]
  where
    h =
      hashSalt 0x9150a946c4a8966e value

    h1  =
      fromIntegral (h `shiftR` 32) .&. maxBound

    -- UHU?! How does this extract the lowest 32 bits?
    -- Guess: h is Word64 but we return Word32 so `fromIntegral`
    -- here truncates the last 32 bits
    h2  =
      fromIntegral h

    num =
      fromIntegral numHashes
