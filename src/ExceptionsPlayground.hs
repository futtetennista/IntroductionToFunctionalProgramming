module ExceptionsPlayground
where

import Prelude hiding (withFile)
import Control.Exception.Safe (mask, finally, onException)
import System.IO hiding (withFile)


-- https://haskell-lang.org/library/safe-exceptions
-- https://haskell-lang.org/tutorial/exception-safety
withFile :: FilePath -> (Handle -> IO ()) -> IO ()
withFile fp inner =
  mask $ \restore -> do
    h <- openFile fp ReadMode
    restore (inner h) `finally` hClose h


with2Files :: FilePath -> FilePath -> (Handle -> Handle -> IO ()) -> IO ()
with2Files fp1 fp2 inner =
  mask $ \restore -> do
    (h1, h2) <- open2Files
    restore $ close2Files (inner h1 h2) h1 h2
      where
        close2Files action h1 h2 =
          action `finally` hClose h1 `finally` hClose h2

        open2Files = do
          h1 <- openFile fp1 ReadMode
          h2 <- openFile fp2 ReadMode `onException` hClose h1
          return (h1, h2)
