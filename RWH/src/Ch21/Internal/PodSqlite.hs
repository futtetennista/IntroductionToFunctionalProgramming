{-# LANGUAGE Strict #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Ch21.Internal.PodSqlite ( Podcast(..)
                               , Episode(..)
                               , addPodcast
                               , addEpisode
                               , updatePodcast
                               , updateEpisode
                               , removePodcast
                               , removeEpisode
                               , getPodcasts
                               , getEpisodes
                               )

where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T
import Control.Monad (when)
import Data.Maybe (isJust)


{-
Reference SQL:

CREATE TABLE podcasts (\
                       \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \castURL TEXT NOT NULL UNIQUE)
CREATE TABLE episodes (\
                       \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \epcastid INTEGER NOT NULL,\
                       \epurl TEXT NOT NULL,\
                       \epdone INTEGER NOT NULL,\
                       \UNIQUE(epcastid, epurl),\
                       \UNIQUE(epcastid, epid))
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Podcast sql=podcasts
  title T.Text
  url T.Text
  UniquePodcast title url
  deriving Eq Show


Episode sql=episodes
  title T.Text
  url T.Text
  mediaUrl T.Text
  done Bool
  podcast PodcastId
  UniqueEpisode podcast url
  UniqueMediaUrl mediaUrl
  -- sqlite3 fk support needs tp be activated at runtime (https://sqlite.org/foreignkeys.html): how do I achieve it?
  -- Foreign Podcast fkpodcast podcast -- how do I declare a fk if the field does not exist in the hs world ?! I have to create a pk field in hs just for that reason but then I have to handle uniqueness in the hs world
  deriving Eq Show
|]


addPodcast :: Podcast -> IO (Maybe Podcast)
addPodcast p =
  runSqlite "db1" $ do
    -- rawQuery "PRAGMA foreign_keys = ON" []
    runMigration migrateAll
    get =<< insert p


updatePodcast :: Podcast -> Podcast -> IO ()
updatePodcast old new =
  runSqlite "db1" $ do
    runMigration migrateAll
    maybe (return ()) updatePodcast' =<< getByValue old
    where
      updatePodcast' e =
        update (entityKey e) [ PodcastTitle =. podcastTitle new
                             , PodcastUrl =. podcastUrl new
                             ]


removePodcast :: Podcast -> IO ()
removePodcast p =
  runSqlite "db1" $ do
    runMigration migrateAll
    deleteEpisodes
    deleteBy $ UniquePodcast (podcastTitle p) (podcastUrl p)
    where
      -- how can I use deleteCascade ?
      deleteEpisodes =
        maybe (return ()) deleteEpisodes' =<< getByValue p

      deleteEpisodes' e =
        deleteWhere [EpisodePodcast ==. entityKey e]


getPodcasts :: IO [Podcast]
getPodcasts =
  runSqlite "db1" $ do
    runMigration migrateAll
    mapM (return . entityVal) =<< selectList [] []


-- This would require a change to this function every time the Episode type is changed
-- addEpisode :: Podcast -> T.Text -> Bool -> IO (Maybe Episode)


addEpisode :: Podcast -> (Key Podcast -> Episode) -> IO ()
addEpisode p mkE =
  runSqlite "db1" $ do
    runMigration migrateAll
    maybe (return ()) addEpisode' =<< getPodcast
    where
      getPodcast =
        getByValue p

      addEpisode' k = do
        let episode =
              mkE (entityKey k)
        mep <- getByValue episode
        maybe (insert_ episode) (flip replace episode . entityKey) mep


updateEpisode :: Episode -> Episode -> IO ()
updateEpisode old new =
  runSqlite "db1" $ do
    runMigration migrateAll
    maybe (return ()) updateEpisode' =<< getByValue old
      where
        updateEpisode' e =
          update (entityKey e) [ EpisodeTitle =. episodeTitle new
                               , EpisodeUrl =. episodeUrl new
                               , EpisodeMediaUrl =. episodeMediaUrl new
                               , EpisodeDone =. episodeDone new
                               , EpisodePodcast =. episodePodcast new
                               ]


removeEpisode :: Episode -> IO ()
removeEpisode e =
  runSqlite "db1" $ do
    runMigration migrateAll
    deleteBy $ UniqueEpisode (episodePodcast e) (episodeUrl e)


getEpisodes :: Podcast -> IO [Episode]
getEpisodes p =
  runSqlite "db1" $ do
    runMigration migrateAll
    maybe (return []) getEpisodes' =<< getByValue p
      where
        getEpisodes' e =
          mapM (return . entityVal) =<< selectList [EpisodePodcast ==. entityKey e] []
