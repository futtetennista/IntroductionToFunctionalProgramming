{-# LANGUAGE OverloadedStrings #-}
module RWH.Ch8.Glob ( namesMatching )
where


import Control.Applicative ((<|>))
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>), pathSeparator)
import System.Posix.Files (fileExist)
import Control.Exception (IOException, handle)
import Control.Monad (forM, filterM)
import RWH.Ch8.GlobRegex (matchesGlob')
import Data.Foldable
import qualified Data.ByteString.Char8 as C8 (pack)


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
                                     return $ (map (dir </>) baseNames) ++ matches)
            [] dirs


-- Ex. 2
doesNameExist :: FilePath -> IO Bool
doesNameExist =
  fileExist
  -- doesDirectoryExist xs <|> doesFileExist xs


listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName then getCurrentDirectory else return dirName
  handle emptyResults (listMatches' dirName')
  where
    emptyResults :: IOException -> IO [String]
    emptyResults =
      const (return [])

    listMatches' :: FilePath -> IO [String]
    listMatches' dirName' = do
      names <- getDirectoryContents dirName'
      let names' =
            if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
          pat' =
            C8.pack pat
          -- Ex. 1
          isWindows =
            pathSeparator ==  '\\'
          caseSensitive =
            not isWindows
      foldrM (\name matches -> do match <- matchesGlob' name pat' caseSensitive
                                  return (if match then (name:matches) else matches))
        [] names'
      -- filterMatch pat' names'

    -- filterMatch _ [] =
    --   return []
    -- filterMatch regex (name:names) = do
    --   match <- matchesGlob' name regex True
    --   xs <- filterMatch regex names
    --   return (if match then (name:xs) else xs)


isHidden :: [Char] -> Bool
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
