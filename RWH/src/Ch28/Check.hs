#!/usr/bin/env stack
{-
stack --resolver lts-8.6 script
--package stm
--package mtl
--package network-uri
--package http-client
--package http-client-tls
--package http-types
--package bytestring
--package conduit-combinators
--package containers
--package async
--package optparse-applicative
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- module Ch28.Check
-- where


import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (IOException, catch)
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isControl)
import Network.URI (URI, parseURI)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as StrictBS
import qualified Data.Set as Set
import Network.HTTP.Client ( Manager, Response
                           , newManager, httpNoBody, parseRequest, responseStatus
                           , responseHeaders
                           )
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Conduit
import Options.Applicative ( (<**>), Parser, ParserInfo
                           , many, strOption, metavar, option, auto, short
                           , value, help, info, helper, fullDesc, progDesc
                           , header, execParser)
import Data.Semigroup ((<>))


type URL =
  StrictBS.ByteString


data Task
  = Check URL
  | Done


main :: IO ()
main = do
  opts <- execParser p_opts
  let
    numFiles =
      length (optFiles opts)

    N k =
      optFlag opts
  badCount <- newTVarIO (0 :: Int)
  badLinks <- newTChanIO
  jobQueue <- newTChanIO
  _badLinksWriter <- withAsync (return ()) $
    \a -> do _ <- wait a ; writeBadLinks badLinks
  manager <- newManager tlsManagerSettings
  workers <- replicateTimes k (worker manager badLinks jobQueue badCount)
  stats <- execJob (mapM_ checkURLs (optFiles opts))
                   (JobState Set.empty 0 jobQueue)
  atomically $ replicateM_ k (writeTChan jobQueue Done)
  waitAll workers
  broken <- readTVarIO badCount
  printf fmt broken
             (linksFound stats)
             (Set.size (linksSeen stats))
             numFiles
  where
    fmt =
      "Found %d broken links. Checked %d links (%d unique) in %d files.\n"


replicateTimes :: Int -> IO () -> IO [Async ()]
replicateTimes n action =
  forM [1..n] $ const (async action)


-- Wait for all workers to be done, ignoring failures
waitAll :: [Async a] -> IO ()
waitAll [] =
  return ()
waitAll (a:as) =
  waitCatch a *> waitAll as


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
          checkOne (StrictBS.unpack x) >> loop

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
checkURLs fp =
  Job $
    runConduitRes $ sourceFileBS fp
      .| mapC extractLinks
      .| setupJob
  where
    setupJob :: Consumer [URL] (ResourceT (StateT JobState IO)) ()
    setupJob =
      (getZipConduit $
        ZipConduit (filterMCE seenURI .| mapM_C enqueueTasks)
        <* ZipConduit (mapM_C (updateStats . length)))


updateStats :: (MonadState JobState m) => Int -> m ()
updateStats n =
  modify $ \s ->
    s { linksFound = linksFound s + n }


enqueueTasks :: (MonadState JobState m, MonadIO m) => [URL] -> m ()
enqueueTasks urls = do
  task <- gets linkQueue
  liftIO . atomically $ mapM_ (writeTChan task . Check) urls


insertURI :: (MonadState JobState m) => URL -> m ()
insertURI c =
  modify $ \s ->
    s { linksSeen = Set.insert c (linksSeen s) }


seenURI :: (MonadState JobState m) => URL -> m Bool
seenURI url = do
  newUrl <- (not . Set.member url) <$> gets linksSeen
  insertURI url
  return newUrl


extractLinks :: StrictBS.ByteString -> [URL]
extractLinks =
  concatMap uris . StrictBS.lines
  where
    uris s =
      filter httpSchemes (StrictBS.splitWith isDelim s)

    isDelim c =
      isControl c || c `elem` (" <>\"{}|\\^[]`" :: String)

    httpSchemes s =
      "http:" `StrictBS.isPrefixOf` s || "https:" `StrictBS.isPrefixOf` s


-- COMMAND LINE PARSING
newtype Flag =
  N Int
  deriving Eq


data Options =
  Options { optFiles :: [FilePath]
          , optFlag :: !Flag
          }


p_options :: Parser Options
p_options =
  Options <$> p_files <*> p_flag
  where
    p_files =
      many $ strOption (metavar "FILEPATH")


p_flag :: Parser Flag
p_flag =
  N <$> option auto (short 'n'
                     <> value 16
                     <> help "Number of concurrent connections (defaults to 16)")


p_opts :: ParserInfo Options
p_opts =
  info (p_options <**> helper)
    (fullDesc
     <> progDesc "Check hyperlinks contained in [FILEPATH ...]"
     <> header "urlcheck - a hyperlink checker")
