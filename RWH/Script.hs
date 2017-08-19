#!/usr/bin/env stack
-- stack script --resolver lts-8.6 --package bytestring, directory
{-# LANGUAGE OverloadedStrings #-}

import System.Directory (listDirectory)
import System.IO
import Control.Monad (mapM_, forM_)
import qualified Data.List as L (isSuffixOf)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  chapterDirs <- listDirectory "./tmp"
  forM_ (map (("./src/")++) chapterDirs) readFiles
  where
    readFiles :: FilePath -> IO ()
    readFiles dir = do
      fs <- listDirectory dir
      let hsFiles = filter (L.isSuffixOf ".hs") fs
      forM_ hsFiles $ \file -> withFile file ReadWriteMode modifyModuleName

    modifyModuleName h = do
      xs <- BS.hGetLine h
      case BS.stripPrefix "module RWH." xs of
        Nothing ->
          BS.hPut h xs >> modifyModuleName h

        Just remainder ->
          BS.hPut h ("module " `BS.append` remainder)
