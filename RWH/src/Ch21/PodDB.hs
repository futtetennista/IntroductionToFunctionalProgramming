module Ch21.PodDB ( Podcast(..)
                  , Episode(..)
                  , Key
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

import Ch21.Internal.PodSqlite
import Database.Persist (Key)
