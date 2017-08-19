module RWH.Ch9.FoldDir
where

import Data.Char (toLower)
import System.FilePath ((</>), takeFileName, takeExtension)
import RWH.Ch9.ControlledVisit (Info(..), TraverseOrder, getUsefulContents, getInfo, isDirectory, preOrder, postOrder)


data Iterate seed
  = Done     { unwrap :: seed }
  | Skip     { unwrap :: seed }
  | Continue { unwrap :: seed }
  deriving Show


type Iterator seed a =
  seed -> a -> Iterate seed


foldTree :: TraverseOrder -> Iterator a Info -> a -> FilePath -> IO a
foldTree order iter initSeed path = -- do
  -- endSeed <- fold initSeed path
  -- return (unwrap endSeed)
  (return . unwrap) =<< fold initSeed path
  where
    fold seed subpath =
      walk seed =<< orderedContents subpath

    orderedContents :: FilePath -> IO [Info]
    orderedContents subpath = do
      names <- getUsefulContents subpath
      contents <- mapM getInfo (map (subpath </>) names)
      return (order contents)

    -- walk :: a -> [Info] -> IO (Iterate a)
    walk seed (info:infos) =
      case iter seed info of
        done@(Done _) ->
          return done

        Skip seed' ->
          walk seed' infos

        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path
              case next of
                done@(Done _) ->
                  return done

                seed'' ->
                  walk (unwrap seed'') infos
          | otherwise ->
            walk seed' infos
    walk seed _ =
      return (Continue seed)


foldTreePreOrder :: Iterator a Info -> a -> FilePath -> IO a
foldTreePreOrder =
  foldTree preOrder


foldTreePostOrder :: Iterator a Info -> a -> FilePath -> IO a
foldTreePostOrder =
  foldTree postOrder


-- ITERATORS
atMostThreePictures :: Iterator [FilePath] Info
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


countDirectories :: Iterator Int Info
countDirectories n info =
  Continue (if isDirectory info then n + 1 else n)
