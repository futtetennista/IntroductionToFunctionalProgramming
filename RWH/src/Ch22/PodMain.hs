module Ch22.PodMain
where

import Ch22.PodDownload (mkManager, downloadURL, updatePodcastFromFeed, getEpisode)
import Ch21.PodDB (Podcast(..), Episode(..), addPodcast, getPodcasts, getEpisodes)
import Ch22.PodParser (Feed(..), parse')
import System.Environment (getArgs)
import Network.Socket (withSocketsDo)
import Network.HTTP.Client (Manager)
import Data.Either (either)
import qualified Data.Text as T


main :: IO ()
main =
  withSocketsDo $ do
    args <- getLine -- test it on ghci for now
    manager <- mkManager
    case words args of
      ["add", url] ->
        add manager (T.pack url)

      ["update"] ->
        update manager

      ["download"] ->
        download manager

      ["fetch"] -> do
        update manager
        download manager

      _ ->
        syntaxError


retrieveFeed :: Manager -> T.Text -> IO (Maybe Feed)
retrieveFeed m url = do
  bs <- downloadURL url m
  either (\_ -> return Nothing) (return . flip parse' "new") bs


add :: Manager -> T.Text -> IO ()
add m url =
  maybe (return ()) (\x -> do addPodcast x ; return ()) =<< mpc
  where
    mpc = do
      mfeed <- retrieveFeed m url
      let mpc =
            maybe Nothing (\feed -> Just $ Podcast (channelTitle feed) url) mfeed
      return mpc


update :: Manager -> IO ()
update m =
  mapM_ procPodcast =<< getPodcasts
  where
    procPodcast pc = do
      putStrLn ("Updating from: " ++ T.unpack (podcastUrl pc))
      updatePodcastFromFeed pc m


download :: Manager -> IO ()
download m =
  mapM_ procPodcast =<< getPodcasts
  where
    procPodcast pc = do
      putStrLn $ "Considering " ++ T.unpack (podcastUrl pc)
      eps <- getEpisodes pc
      let pendingEps =
            filter (\ep -> episodeDone ep == False) eps
      mapM_ procEpisode pendingEps

    procEpisode ep = do
      putStrLn $ "Downloading " ++ T.unpack (episodeUrl ep)
      getEpisode ep m


syntaxError =
  putStrLn "Usage: pod command [args]\n\
           \\n\
           \pod add url      Adds a new podcast with the given URL\n\
           \pod download     Downloads all pending episodes\n\
           \pod fetch        Updates, then downloads\n\
           \pod update       Downloads podcast feeds, looks for new episodes\n"
