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
import Text.Parsec


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
      _ <- statefulParser (char ':')
      fcontents <- fieldContents (contentParser fname)
      return (fname, fcontents)
      where
        contentParser name =
          if fmap toLower name == "content-length" then digit else notEOL

    fieldName :: Monad m => ParsecT String Int m String
    fieldName =
      (:) <$> statefulParser letter <*> nameTail
      where
        nameTail =
          (many . statefulParser) (letter <|> digit <|> oneOf "-_")

    statefulParser :: Monad m => ParsecT s Int m a -> ParsecT s Int m a
    statefulParser p = do
      xs <- boundedParser
      modifyState (+1)
      return xs
      where
        boundedParser = do
          headerLength <- getState
          if headerLength < 4096 then p else parserFail "Header line too big"

    fieldContents :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    fieldContents contentParser =
      (many . statefulParser) space *> (contents . statefulParser) contentParser

    contents :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    contents p =
      liftA2 (++) (many1 p <* crlf) (continuation p <|> pure [])

    continuation :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    continuation p =
      liftA2 (:) (' ' <$ many1 (statefulParser . oneOf $ " \t")) (contents p)
