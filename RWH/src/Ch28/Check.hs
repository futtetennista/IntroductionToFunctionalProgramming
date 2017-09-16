#!/usr/bin/env stack
{-
stack --resolver lts-8.6
--package stm
--package network-uri
--package http-client
--package http-client-tls
--package http-types
--package bytestring
--package conduit
--package containers
--package async
--package optparse-applicative
script
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Ch28.Check
where


import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (IOException, catch, finally)
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isControl)
import Data.List (nub)
import Network.URI (URI, parseURI)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import qualified Data.ByteString.Char8 as StrictBS
import qualified Data.Set as Set
import Network.HTTP.Client ( Manager, Response
                           , newManager, httpNoBody, parseRequest, responseStatus
                           , responseHeaders
                           )
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Client.TLS (tlsManagerSettings)


type URL =
  LazyBS.ByteString


data Task
  = Check URL
  | Done


main :: IO ()
main = do
  (files, k) <- parseArgs
  let
    numFiles =
      length files
  badCount <- newTVarIO (0 :: Int)
  badLinks <- newTChanIO
  jobQueue <- newTChanIO
  workers <- newTVarIO k
  _ <- withAsync (writeBadLinks badLinks) $ const (return ())
  manager <- newManager tlsManagerSettings
  replicateTimes k workers (worker manager badLinks jobQueue badCount)
  stats <- execJob (mapM_ checkURLs files)
                   (JobState Set.empty 0 jobQueue)
  atomically $ replicateM_ k (writeTChan jobQueue Done)
  waitFor workers
  -- is atomically really needed here? All workers are done by this time
  broken <- readTVarIO badCount
  printf fmt broken
             (linksFound stats)
             (Set.size (linksSeen stats))
             numFiles
  where
    fmt =
      "Found %d broken links. Checked %d links (%d unique) in %d files.\n"


replicateTimes :: Int -> TVar Int -> IO () -> IO ()
replicateTimes k alive action =
  replicateConcurrently_ k $ do
    action `finally` (atomically $ modifyTVar' alive (subtract 1))


writeBadLinks :: TChan String -> IO ()
writeBadLinks c =
  forever $
    atomically (readTChan c) >>= putStrLn >> hFlush stdout


waitFor :: TVar Int -> IO ()
waitFor alive =
  atomically $ do
    count <- readTVar alive
    check (count == 0)


-- CHECKING LINKS
getStatusE :: Manager -> URI -> IO (Either String Int)
getStatusE m =
  runExceptT . chase (5 :: Int)
  where
    chase :: Int -> URI -> ExceptT String IO Int
    chase 0 _ =
      throwError "too many redirects"
    chase n u = do
      r <- embedEither show =<< liftIO (getHead u)
      case statusIsRedirection (responseStatus r) of
        True -> do
          u'  <- embedMaybe (show r) $ findHeader hLocation
          url <- embedMaybe "bad URL" $ parseURI (StrictBS.unpack u')
          chase (n - 1) url
            where
              findHeader name =
                lookup name (responseHeaders r)

        False ->
          return . statusCode . responseStatus $ r

    getHead :: URI -> IO (Either StrictBS.ByteString (Response ()))
    getHead uri = do
      request <- parseRequest ("HEAD " ++ show uri)
      response <- httpNoBody request m
      let
        status =
          responseStatus response
      if | statusIsSuccessful status ->
             return $ Right response
         | statusIsRedirection status ->
             return $ Right response
         | otherwise ->
             return . Left . statusMessage . responseStatus $ response


embedEither :: (MonadError e m) => (s -> e) -> Either s a -> m a
embedEither f =
  either (throwError . f) return


embedMaybe :: (MonadError e m) => e -> Maybe a -> m a
embedMaybe err =
  maybe (throwError err) return


worker :: Manager -> TChan String -> TChan Task -> TVar Int -> IO ()
worker m badLinks jobQueue badCount =
  loop
  where
    loop = do
      job <- atomically $ readTChan jobQueue
      case job of
        Done ->
          return ()

        Check x ->
          checkOne (LazyBS.unpack x) >> loop

    checkOne :: String -> IO ()
    checkOne url =
      case parseURI url of
        Just uri -> do
          code <- getStatusE m uri `catch`
            (return . Left . show :: IOException -> IO (Either String Int))
          case code of
            Right 200 ->
              return ()

            Right n ->
              report (show n)

            Left err ->
              report err

        _ ->
          report "invalid URL"
        where
          report :: String -> IO ()
          report s =
            atomically $ do
              modifyTVar' badCount (+1)
              writeTChan badLinks (url ++ " " ++ s)


-- FINDING LINKS
data JobState =
  JobState { linksSeen :: Set.Set URL
           , linksFound :: Int
           , linkQueue :: TChan Task
           }


newtype Job a =
  Job { runJob :: StateT JobState IO a }
  deriving (Functor, Applicative, Monad, MonadState JobState, MonadIO)


execJob :: Job a -> JobState -> IO JobState
execJob =
  execStateT . runJob


checkURLs :: FilePath -> Job ()
checkURLs f = do
  src <- liftIO $ LazyBS.readFile f
  let
    urls =
      extractLinks src
  filterM seenURI urls >>= sendJobs
  updateStats (length urls)


updateStats :: Int -> Job ()
updateStats n =
  modify $ \s ->
    s { linksFound = linksFound s + n }


insertURI :: URL -> Job ()
insertURI c =
  modify $ \s ->
    s { linksSeen = Set.insert c (linksSeen s) }


seenURI :: URL -> Job Bool
seenURI url = do
  seen <- (not . Set.member url) <$> gets linksSeen
  insertURI url
  return seen


sendJobs :: [URL] -> Job ()
sendJobs urls = do
  c <- gets linkQueue
  liftIO . atomically $ mapM_ (writeTChan c . Check) urls


extractLinks :: LazyBS.ByteString -> [URL]
extractLinks =
  concatMap uris . LazyBS.lines
  where
    uris s =
      filter looksOkay (LazyBS.splitWith isDelim s)

    isDelim c =
      isControl c || c `elem` (" <>\"{}|\\^[]`" :: String)

    looksOkay s =
      http `LazyBS.isPrefixOf` s

    http =
      "http:"

-- COMMAND LINE PARSING
data Flag
  = Help
  | N Int
  deriving Eq


parseArgs :: IO ([String], Int)
parseArgs = do
  argv <- getArgs
  case parse argv of
    ([], files, []) ->
      return (nub files, 16)

    (opts, files, [])
      | Help `elem` opts ->
          help
      | [N n] <- filter (/=Help) opts ->
          return (nub files, n)

    (_, _, errs) ->
      die errs
  where
    parse argv =
      getOpt Permute options argv

    header =
      "Usage: urlcheck [-h] [-n n] [file ...]"

    info =
      usageInfo header options

    dump =
      hPutStrLn stderr

    die errs =
      dump (concat errs ++ info) >> exitWith (ExitFailure 1)

    help =
      dump info >> exitWith ExitSuccess


options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "Show this help message"
  , Option ['n'] [] (ReqArg (\s -> N (read s)) "N") $
      "Number of concurrent connections (default 16)"
  ]
