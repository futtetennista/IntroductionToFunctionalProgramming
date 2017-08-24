{-# LANGUAGE OverloadedStrings #-}
module Ch22.PodDownload

where

import Ch21.PodDB
import Ch22.PodParser
import qualified Data.Text as T (Text, pack, unpack, splitOn)
import qualified Data.Text.Lazy as TL (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8')
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Client ( Manager
                           , newManager
                           , defaultManagerSettings
                           , httpLbs
                           , parseRequest
                           , responseBody
                           , responseStatus
                           )
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad (forM_)
import System.IO (IOMode(WriteMode), openBinaryFile, hClose)
import Control.Exception (bracket, catch)


type ErrorMsg =
  T.Text


mkManager :: IO Manager
mkManager =
  newManager tlsManagerSettings


downloadURL :: T.Text -> Manager -> IO (Either ErrorMsg LB.ByteString)
downloadURL url m = do
  request <- parseRequest ("GET " ++ T.unpack url)
  response <- httpLbs request m
  case responseStatus response of
    status200 ->
      return . Right . responseBody $ response


{- | Update the podcast in the database. -}
updatePodcastFromFeed :: Podcast -> Manager -> IO ()
updatePodcastFromFeed p m = do
  efeed <- downloadURL (podcastUrl p) m
  either (\_ -> return ()) updatePodcastFromFeed' efeed
  where
    updatePodcastFromFeed' response =
      case decodeUtf8' response of
        Right content -> do
          let feed =
                parse (TL.toStrict content) (podcastTitle p)
              p' =
                p { podcastTitle = channelTitle feed }

          updatePodcast p p'
          forM_ (items feed) (addEpisode p' . itemToEp)

        Left err ->
          return ()


{- | Downloads an episode, returning a String representing
the filename it was placed into, or Nothing on error. -}
getEpisode :: Episode -> Manager -> IO (Maybe T.Text)
getEpisode ep m = do
  resp <- downloadURL (episodeMediaUrl ep) m
  either (const (return Nothing)) saveEpisode resp
  where
    saveEpisode :: LB.ByteString -> IO (Maybe T.Text)
    saveEpisode bytes =
      catch (fmap (Just . T.pack) $ saveMedia bytes)
            ((\_ -> return Nothing) :: IOError -> IO (Maybe T.Text))

    extractFileName :: T.Text -> FilePath
    extractFileName =
      T.unpack . last . T.splitOn "/"

    saveMedia :: LB.ByteString -> IO FilePath
    saveMedia bytes = do
      let fileName =
            extractFileName . episodeUrl $ ep
      bracket (openBinaryFile fileName WriteMode) hClose (writeToFile bytes)
      updateEpisode ep (ep { episodeDone = True })
      return fileName

    writeToFile =
      flip LB.hPut
