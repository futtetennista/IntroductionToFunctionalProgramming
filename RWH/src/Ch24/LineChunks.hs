module Ch24.LineChunks (chunkedReadWith)

where

import Control.Exception (bracket, finally)
import Control.Monad (forM)
import Control.Parallel.Strategies (NFData, using, rdeepseq)
import Control.DeepSeq (rnf)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Conc (numCapabilities)
import System.IO


data ChunkSpec =
  CS { chunkOffset :: !Int64
     , chunkLength :: !Int64
     }
  deriving (Eq, Show)


chunkedReadWith :: (NFData a)
                => ([LB.ByteString] -> a)
                -> FilePath
                -> IO a
chunkedReadWith func path =
  withChunks (lineChunks (numCapabilities * 4)) func path


withChunks :: (NFData a)
           => (FilePath -> IO [ChunkSpec])
           -> ([LB.ByteString] -> a)
           -> FilePath
           -> IO a
withChunks chunkFunc process path = do
  (chunks, handles) <- chunkedRead chunkFunc path
  let res =
        process chunks
  -- make sure computation is evaluated **before** closing the handles
  (rnf res `seq` return res) `finally` mapM_ hClose handles
  -- return (process chunks `using` rdeepseq) `finally` mapM_ hClose handles


chunkedRead :: (FilePath -> IO [ChunkSpec])
            -> FilePath
            -> IO ([LB.ByteString], [Handle])
chunkedRead chunkFunc path = do
  chunkSpecs <- chunkFunc path
  fmap unzip . forM chunkSpecs $ \spec -> do
    h <- openFile path ReadMode
    hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
    chunk <- LB.take (chunkLength spec) <$> LB.hGetContents h
    return (chunk, h)


lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
  bracket (openFile path ReadMode) hClose $ \h -> do
    totalSize <- fromIntegral <$> hFileSize h
    let
      chunkSize =
        totalSize `div` fromIntegral numChunks

      findChunks :: Int64 -> IO [ChunkSpec]
      findChunks offset = do
        let
          newOffset =
            offset + chunkSize
        hSeek h AbsoluteSeek (fromIntegral newOffset)

        let
          findNewline :: Int64 -> IO [ChunkSpec]
          findNewline off = do
            eof <- hIsEOF h
            if eof
              then return [CS offset (totalSize - offset)]
              else do bytes <- LB.hGet h 4096
                      case LB.elemIndex '\n' bytes of
                        Just n -> do
                          chunks@(c:_) <- findChunks (off + n + 1)
                          let coff = chunkOffset c
                          return $ CS offset (coff - offset) : chunks

                        Nothing ->
                          findNewline (off + LB.length bytes)
        findNewline newOffset

    findChunks 0
