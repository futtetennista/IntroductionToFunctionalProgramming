module Ch22.PodTypes

where

import Data.Text (Text)


data Podcast =
  Podcast { castId :: Integer
          , castURL :: Text
          }
  deriving (Eq, Show, Read)


data Episode =
  Episode { epId :: Integer
          , epCast :: Podcast
          , epURL :: Text
          , epDone :: Bool
          }
  deriving (Eq, Show, Read)
