module RWH.Ch16.HttpRequestParser ( HttpRequest(..)
                                  , Method(..)
                                  , Header
                                  , p_request
                                  , p_headers
                                  --, p_query
                                  )
where


import Data.Char (toLower)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Numeric (readHex)
import System.IO (Handle)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim (Parsec)


data Method
  = Get
  | Post
  deriving (Eq, Ord, Show)


type Header =
  (String, String)


data HttpRequest =
  HttpRequest { reqMethod :: Method
              , reqURL :: String
              , reqHeaders :: [Header]
              , reqBody :: Maybe String
              }
  deriving (Eq, Show)


p_request :: Monad m => ParsecT String Int m HttpRequest
p_request =
  q "GET" Get (pure Nothing)
  <|> q "POST" Post (Just <$> many anyChar)
  where
    q :: Monad m => String -> Method -> ParsecT [Char] Int m (Maybe String) -> ParsecT [Char] Int m HttpRequest
    q name ctor body =
      HttpRequest <$> req <*> url <*> p_headers <*> body
      where
        req =
          ctor <$ string name <* char ' '

    url :: Monad m => ParsecT [Char] a m String
    url =
      optional (char '/')
      *> manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
      <* crlf


notEOL :: Monad m => ParsecT String a m Char
notEOL =
  noneOf "\r\n"


p_headers :: Monad m => ParsecT String Int m [Header]
p_headers =
  header `manyTill` crlf
  where
    header :: Monad m => ParsecT String Int m Header
    header = do
      fname <- fieldName
      _ <- statefulParse (char ':')
      fcontents <- fieldContents (contentParser fname)
      return (fname, fcontents)
      where
        contentParser name =
          if fmap toLower name == "content-length" then digit else notEOL

    fieldName :: Monad m => ParsecT String Int m String
    fieldName =
      (:) <$> statefulParse letter <*> boundedMany fieldChar

    boundedMany1 :: Monad m => ParsecT s Int m a -> ParsecT s Int m [a]
    boundedMany1 p = do
      n <- getState
      if n < 4096
        then (:) <$> statefulParse p <*> boundedMany p
        else parserFail "Header line too big"

    statefulParse :: Monad m => ParsecT s Int m a -> ParsecT s Int m a
    statefulParse p = do
      xs <- p
      modifyState (+1)
      return xs

    boundedMany :: Monad m => ParsecT s Int m a -> ParsecT s Int m [a]
    boundedMany p =
      boundedMany1 p <|> pure []

    fieldChar :: Monad m => ParsecT String a m Char
    fieldChar =
      letter <|> digit <|> oneOf "-_"

    fieldContents :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    fieldContents contentParser =
      boundedMany space *> contents contentParser

    contents :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    contents parser =
      liftA2 (++) (boundedMany1 parser <* crlf) (continuation parser <|> pure [])

    continuation :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    continuation parser =
      liftA2 (:) (' ' <$ boundedMany1 (oneOf " \t")) (contents parser)
