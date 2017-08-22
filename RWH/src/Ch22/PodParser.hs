module Ch22.PodParser

where

import Ch22.PodTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Generate (showattr)
import Text.XML.HaXml.Posn (Posn, noPos)
import Data.Char
import qualified Data.Text as T


data PodItem =
  PodItem { itemtitle :: T.Text
          , enclosureurl :: T.Text
          }
  deriving (Eq, Show, Read)


data Feed =
  Feed { channeltitle :: T.Text
       , items :: [PodItem]
       }
  deriving (Eq, Show, Read)


item2ep :: Podcast -> PodItem -> Episode
item2ep pc item =
  Episode { epId = 0
          , epCast = pc
          , epURL = enclosureurl item
          , epDone = False
          }


sampleFeed :: T.Text
sampleFeed =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss xmlns:itunes=\"http://www.itunes.com/DTDs/Podcast-1.0.dtd\" version=\"2.0\"><channel><title>Haskell Radio</title><link>http://www.example.com/radio/</link><description>Description of this podcast</description><item><title>Episode 2: Lambdas</title><link>http://www.example.com/radio/lambdas</link><enclosure url=\"http://www.example.com/radio/lambdas.mp3\" type=\"audio/mpeg\" length=\"10485760\"/></item><item><title>Episode 1: Parsec</title><link>http://www.example.com/radio/parsec</link><enclosure url=\"http://www.example.com/radio/parsec.mp3\" type=\"audio/mpeg\" length=\"10485150\"/></item></channel></rss>"


parse :: T.Text -> T.Text -> Feed
parse content name =
  Feed { channeltitle = getTitle doc
       , items = getEnclosures doc
       }
  where
    parseResult :: Document Posn
    parseResult =
      xmlParse (T.unpack name) (T.unpack (stripUnicodeBOM content))

    doc :: Content Posn
    doc =
      getContent parseResult

    getContent :: Document Posn -> Content Posn
    getContent (Document _ _ e _) =
      CElem e noPos

    stripUnicodeBOM :: T.Text -> T.Text
    stripUnicodeBOM xs =
      maybe xs id (T.stripPrefix "\xef\xbb\xbf" xs)


channel :: CFilter a
channel =
  tag "rss" /> tag "channel"


getTitle :: Content a -> T.Text
getTitle =
  contentToStringDefault "Untitled Podcast" . (channel /> tag "title" /> txt)


getEnclosures :: Content a -> [PodItem]
getEnclosures =
  concatMap procPodItem . getPodItems
  where
    procPodItem :: Content a -> [PodItem]
    procPodItem item =
      concatMap (procEnclosure title) enclosure
      where
        title =
          contentToStringDefault "Untitled Episode" (keep /> tag "title" /> txt $ item)

        enclosure =
          (keep /> tag "enclosure") item

    getPodItems :: CFilter a
    getPodItems =
      channel /> tag "item"

    procEnclosure :: T.Text -> Content a -> [PodItem]
    procEnclosure title enclosure =
      map mkPodItem (showattr "url" enclosure)
      where
        mkPodItem :: Content a -> PodItem
        mkPodItem x =
          PodItem { itemtitle = title
                  , enclosureurl = contentToString [x]
                  }


contentToStringDefault :: T.Text -> [Content a] -> T.Text
contentToStringDefault msg [] =
  msg
contentToStringDefault _ x =
  contentToString x


contentToString :: [Content a] -> T.Text
contentToString =
  T.pack . concatMap procContent
  where
    procContent :: Content a -> String
    procContent x =
      verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) (info x)

    fakeElem :: Content a -> Element a
    fakeElem x =
      Elem (N "fake") [] [x]

    unesc :: Element a -> Element a
    unesc =
      xmlUnEscape stdXmlEscaper
