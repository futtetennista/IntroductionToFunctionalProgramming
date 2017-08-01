module RWH.Ch9.FoldDir
where

import Data.Char (toLower)
import System.FilePath ((</>), takeFileName, takeExtension)
import RWH.Ch9.ControlledVisit (Info(..), TraverseOrder, getUsefulContents, getInfo, isDirectory)


data Iterate seed
  = Done     { unwrap :: seed }
  | Skip     { unwrap :: seed }
  | Continue { unwrap :: seed }
  deriving (Show)


type Iterator seed =
  seed -> Info -> Iterate seed


foldTree :: Iterator a -> TraverseOrder -> a -> FilePath -> IO a
foldTree iter order initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath =
      walk seed =<< getUsefulContents subpath

    walk seed (name:names) = do
      let fullPath = path </> name
      info <- getInfo fullPath
      case iter seed info of
        done@(Done _) ->
          return done

        Skip seed' ->
          walk seed' names

        Continue seed'
          | isDirectory info -> do
              next <- fold seed' fullPath
              case next of
                done@(Done _) ->
                  return done

                seed'' ->
                  walk (unwrap seed'') names
          | otherwise ->
            walk seed' names
    walk seed _ =
      return (Continue seed)


atMostThreePictures :: [FilePath] -> Info -> Iterate [FilePath]
atMostThreePictures paths info
  | length paths == 3 =
    Done paths
  | isDirectory info && takeFileName path == ".git" =
    Skip paths
  | extension `elem` [".jpg", ".png"] =
    Continue (path : paths)
  | otherwise =
    Continue paths
  where
    extension =
      map toLower (takeExtension path)

    path =
      infoPath info
