module Ch9.ControlledVisit ( Info(..)
                           , TraverseOrder
                           , traverse
                           , traversePreOrder
                           , traversePostOrder
                           , reverseAlphabeticOrder
                           , findContents
                           , getUsefulContents
                           , getInfo
                           , isDirectory
                           , postOrder
                           , preOrder
                           )
where

import Data.Time (UTCTime)
import Data.List (sortBy)
import Control.Arrow ((***))
import Control.Exception.Base (IOException, handle, bracket)
import Control.Monad (liftM, forM)
import Prelude hiding (traverse)
import System.Directory (Permissions, getDirectoryContents, searchable, getPermissions, getModificationTime)
import System.IO (IOMode(ReadMode), openFile, hClose, hFileSize)
import System.FilePath ((</>))


data Info =
  Info { infoPath :: FilePath
       , infoPerms :: Maybe Permissions
       , infoSize :: Maybe Integer
       , infoModTime :: Maybe UTCTime
       }
  deriving (Eq, Ord, Show)


maybeIO :: IO a -> IO (Maybe a)
maybeIO act =
  handle (const (return Nothing) :: IOException -> IO (Maybe a)) (Just `liftM` act)


getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)


type TraverseOrder =
  [Info] -> [Info]


type Sieve =
  Info -> Bool


traverse :: TraverseOrder -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]


getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)


isDirectory :: Info -> Bool
isDirectory =
  maybe False searchable . infoPerms


reverseAlphabeticOrder :: TraverseOrder
reverseAlphabeticOrder =
  -- reverse . sortBy (\x -> uncurry compare . infoPaths x)
  sortBy (\x -> reverseOrd . uncurry compare . infoPaths x)
  where
    infoPaths x y =
      (infoPath x, infoPath y)

    reverseOrd GT =
      LT
    reverseOrd LT =
      GT
    reverseOrd EQ =
      EQ


preOrder :: TraverseOrder
preOrder =
  id


postOrder :: TraverseOrder
postOrder =
  sortBy (\x -> uncurry compare . infoPermss x)
  where
    infoPermss x y =
      (msearchable *** msearchable) (infoPerms x, infoPerms y)

    msearchable =
      maybe False searchable


traversePreOrder :: FilePath -> IO [Info]
traversePreOrder =
  traverse preOrder


traversePostOrder :: FilePath -> IO [Info]
traversePostOrder =
  traverse postOrder


-- Ex: sieveTraverse (not . Data.List.isSuffixOf "foo" . infoPath) id "./tmp/" (filters out all files called "foo" in the "tmp" directory)
findContents :: Sieve -> TraverseOrder -> FilePath -> IO [Info]
findContents sieve order path =
  filter sieve `liftM` traverse order path
