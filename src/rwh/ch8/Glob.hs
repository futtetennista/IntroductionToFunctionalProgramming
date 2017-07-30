{-# LANGUAGE OverloadedStrings #-}
module RWH.Ch8.Glob ( namesMatching )
where


import Control.Applicative ((<|>))
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>), pathSeparator)
import System.Posix.Files (getFileStatus, fileExist, isDirectory)
import Control.Exception (IOException, handle)
import Control.Monad (forM)
import RWH.Ch8.GlobRegex (matchesGlob')
import Data.Foldable
import qualified Data.ByteString as B (ByteString, unpack)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.List as L


isPattern :: String -> Bool
isPattern =
  any (`elem` ("[*?" :: String))


namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) =
    (\flg -> if flg then [pat] else []) <$> doesNameExist pat
  | otherwise =
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName

        (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir =
                if isPattern baseName then listMatches else listPlain
          -- pathNames <- forM dirs $ \dir -> do baseNames <- listDir dir baseName
          --                                     return (map (dir </>) baseNames)
          -- return (concat pathNames)
          foldrM (\dir matches -> do baseNames <- listDir dir baseName
                                     return $ baseNames ++ matches)
            [] dirs


-- Ex. 2
doesNameExist :: FilePath -> IO Bool
doesNameExist =
  fileExist
  -- doesDirectoryExist xs <|> doesFileExist xs


listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName then getCurrentDirectory else return dirName
  handle emptyResults (listMatches' dirName' (C8.pack pat))


emptyResults :: IOException -> IO [String]
emptyResults =
  const (return [])


listMatches' :: FilePath -> B.ByteString -> IO [String]
listMatches' dirName' pat' = do
  names <- getDirectoryContents dirName'
  let names' =
        filter ((if isHidden (C8.unpack pat') then id else not) . isHidden) names
  foldrM allMatches [] names'
  where
    -- Ex. 1
    isWindows =
      pathSeparator ==  '\\'

    -- Ex. 3
    allMatches name matches = do
      (match, rec') <- matchesGlob' name pat' (not isWindows)
      dir <- isDirectory' fullName
      case (match, rec', dir) of
        (True, True, True) -> do
          matches' <- handle emptyResults (listMatches' fullName pat')
          return (fullName : matches' ++ matches)

        (False, True, True) -> do
          matches' <- handle emptyResults (listMatches' fullName pat')
          return (matches' ++ matches)

        (False, _, _) ->
          return matches

        (True, _, _) ->
          return (fullName : matches)
        where
          fullName =
            dirName' </> name

          isDirectory' fileName =
            getFileStatus fileName >>= return . isDirectory


 -- filterMatch pat' matches'

-- filterMatch _ [] =
--   return []
-- filterMatch regex (name:names) = do
--   match <- matchesGlob' name regex True
--   xs <- filterMatch regex names
--   return (if match then (name:xs) else xs)


isHidden :: String -> Bool
isHidden ('.':_) =
  True
isHidden _ =
  False


listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])
