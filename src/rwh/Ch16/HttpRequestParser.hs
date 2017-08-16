module HttpRequestParser ( HttpRequest(..)
                         , Method(..)
                         , p_request
                         --, p_query
                         )
where


import Data.Char (toLower)
import Control.Applicative (liftA2)
import Numeric (readHex)
import Control.Monad (liftM4)
import System.IO (Handle)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim


data Method
  = Get
  | Post
  deriving (Eq, Ord, Show)


data HttpRequest =
  HttpRequest { reqMethod :: Method
              , reqURL :: String
              , reqHeaders :: [(String, String)]
              , reqBody :: Maybe String
              }
  deriving (Eq, Show)


p_request :: Parsec String () HttpRequest
p_request =
  q "GET" Get (pure Nothing)
  <|> q "POST" Post (Just <$> many anyChar)
  where
    q :: String -> Method -> Parsec [Char] a (Maybe String) -> Parsec [Char] a HttpRequest
    q name ctor body =
      liftM4 HttpRequest req url p_headers body
      where
        req =
          ctor <$ string name <* char ' '

    url :: Parsec [Char] a String
    url =
      optional (char '/')
      *> manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
      <* crlf


notEOL :: Parsec String a Char
notEOL =
  noneOf "\r\n"


p_headers :: Parsec String a [(String, String)]
p_headers =
  header `manyTill` crlf
  where
    header = do
      fname <- fieldName
      let contentParser =
            if fmap toLower fname == "content-length" then digit else notEOL
      liftA2 (,) fieldName (char ':' *> spaces *> contents contentParser)

    fieldName =
      (:) <$> letter <*> many fieldChar

    fieldChar =
      letter <|> digit <|> oneOf "-_"

    contents :: Parsec String a Char -> Parsec String a String
    contents parser =
      liftA2 (++) (many1 parser <* crlf) (continuation parser <|> pure [])

    continuation :: Parsec String a Char -> Parsec String a String
    continuation parser =
      liftA2 (:) (' ' <$ many1 (oneOf " \t")) (contents parser)
