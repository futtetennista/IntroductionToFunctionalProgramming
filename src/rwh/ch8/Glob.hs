{-# LANGUAGE OverloadedStrings #-}
module RWH.Ch8.Glob ( namesMatching )
where

import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>), pathSeparator)
import System.Posix.Files (getFileStatus, fileExist, isDirectory)
import Control.Exception (IOException, handle)
import RWH.Ch8.GlobRegex (ErrorMsg, matchesGlob')
import Data.Foldable (foldrM)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.Either as E (either)


isPattern :: String -> Bool
isPattern =
  any (`elem` ("[*?" :: String))


namesMatching :: String -> IO (Either ErrorMsg [String])
namesMatching pat
  | not (isPattern pat) =
    (\flg -> if flg then Right [pat] else Right []) <$> doesNameExist pat
  | otherwise =
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName

        (dirName, baseName) -> do
          edirs <- if isPattern dirName
                   then namesMatching (dropTrailingPathSeparator dirName)
                   else return (Right [dirName])
          case edirs of
            Left err ->
              return (Left err)

            Right dirs ->
              foldrM (nameMatches baseName) (Right []) dirs
      where
        nameMatches baseName dir ematches =
          let listDir =
                if isPattern baseName then listMatches else listPlain
          in
            case ematches of
              Left err ->
                return (Left err)

              Right matches ->
                return . E.either Left (Right . (++matches)) =<< listDir dir baseName


-- Ex. 2
doesNameExist :: FilePath -> IO Bool
doesNameExist =
  fileExist
  -- doesDirectoryExist xs <|> doesFileExist xs


listMatches :: FilePath -> String -> IO (Either ErrorMsg [String])
listMatches dirName pat = do
  dirName' <- if null dirName then getCurrentDirectory else return dirName
  handle emptyResults (listMatches' dirName' (C8.pack pat))


emptyResults :: IOException -> IO (Either ErrorMsg [String])
emptyResults =
  return . Left . show


listMatches' :: FilePath -> B.ByteString -> IO (Either ErrorMsg [String])
listMatches' dirName' pat' = do
  names <- getDirectoryContents dirName'
  let names' =
        filter ((if isHidden (C8.unpack pat') then id else not) . isHidden) names
  foldrM allMatchesWithError (Right []) names'
  where
    -- Ex. 1
    win =
      pathSeparator ==  '\\'

    allMatchesWithError :: FilePath -> Either ErrorMsg [String] -> IO (Either ErrorMsg [String])
    allMatchesWithError name ematches =
      case ematches of
        Left err ->
          return (Left err)

        Right matches ->
          E.either (return . Left) (allMatches name matches) =<< matchesGlob' name pat' (not win)

    -- Ex. 3
    allMatches :: FilePath -> [String] -> (Bool, Bool) -> IO (Either ErrorMsg [String])
    allMatches name matches (match, rec') = do
      dir <- isDirectory' fullName
      case (match, rec', dir) of
        (True, True, True) ->
          return . E.either Left (Right . (++matches) . (fullName:)) =<< listMatches' fullName pat'

        (False, True, True) ->
          return . E.either Left (Right . (++matches)) =<< listMatches' fullName pat'

        (False, _, _) ->
          return (Right matches)

        (True, _, _) ->
          return $ Right (fullName : matches)
        where
          fullName =
            dirName' </> name

          isDirectory' fileName =
            return . isDirectory =<< getFileStatus fileName


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


listPlain :: FilePath -> String -> IO (Either ErrorMsg [String])
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then Right [baseName] else Right [])
