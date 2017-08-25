module Ch22.PodParser

where


import Ch21.PodDB
import Text.XML.HaXml ( (/>) , CFilter , Content(..) , Document(..) , Element(..) , QName (..)
                      , keep, tag, txt, xmlParse, verbatim, info, xmlUnEscape ,stdXmlEscaper)
import Text.XML.HaXml.Html.Generate (showattr)
import Text.XML.HaXml.Posn (Posn, noPos)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8')
import qualified Data.ByteString.Lazy as LB (ByteString)


data PodItem =
  PodItem { itemTitle :: T.Text
          , itemUrl :: T.Text
          , enclosureUrl :: T.Text
          }
  deriving (Eq, Show, Read)


data Feed =
  Feed { channelTitle :: T.Text
       , items :: [PodItem]
       }
  deriving (Eq, Show, Read)


itemToEp :: PodItem -> (Key Podcast -> Episode)
itemToEp item =
  Episode (itemTitle item) (itemUrl item) (enclosureUrl item) False


sampleFeed :: T.Text
sampleFeed =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss xmlns:itunes=\"http://www.itunes.com/DTDs/Podcast-1.0.dtd\" version=\"2.0\"><channel><title>Haskell Radio</title><link>http://www.example.com/radio/</link><description>Description of this podcast</description><item><title>Episode 2: Lambdas</title><link>http://www.example.com/radio/lambdas</link><enclosure url=\"http://www.example.com/radio/lambdas.mp3\" type=\"audio/mpeg\" length=\"10485760\"/></item><item><title>Episode 1: Parsec</title><link>http://www.example.com/radio/parsec</link><enclosure url=\"http://www.example.com/radio/parsec.mp3\" type=\"audio/mpeg\" length=\"10485150\"/></item></channel></rss>"


parse' :: LB.ByteString -> T.Text -> Maybe Feed
parse' bytes name =
  either (const Nothing) (Just . flip parse name . TL.toStrict) (decodeUtf8' bytes)


parse :: T.Text -> T.Text -> Feed
parse content name =
  Feed { channelTitle = title doc
       , items = getEnclosures doc
       }
  where
    title =
      contentToStringDefault "Untitled Podcast" . (channel /> tag "title" /> txt)

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


getEnclosures :: Content a -> [PodItem]
getEnclosures =
  concatMap procPodItem . podItems
  where
    procPodItem :: Content a -> [PodItem]
    procPodItem item =
      concatMap (procEnclosure title link) enclosure
      where
        link =
          contentToString . (keep /> tag "link" /> txt) $ item

        title =
          contentToStringDefault "Untitled Episode" . (keep /> tag "title" /> txt) $ item

        enclosure =
          (keep /> tag "enclosure") item

    podItems :: CFilter a
    podItems =
      channel /> tag "item"

    procEnclosure :: T.Text -> T.Text -> Content a -> [PodItem]
    procEnclosure title link enclosure =
      map mkPodItem (showattr "url" enclosure)
      where
        mkPodItem :: Content a -> PodItem
        mkPodItem x =
          PodItem { itemTitle = title
                  , itemUrl = link
                  , enclosureUrl = contentToString [x]
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
