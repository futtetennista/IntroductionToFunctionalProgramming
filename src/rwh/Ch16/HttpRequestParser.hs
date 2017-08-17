module RWH.Ch16.HttpRequestParser ( HttpRequest(..)
                                  , Method(..)
                                  , Header
                                  , p_request
                                  , p_headers
                                  --, p_query
                                  )
where


import Data.Char (chr, toLower)
import Control.Applicative (liftA, liftA2, liftA3)
import Control.Arrow
import Text.Parsec
import Numeric


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
  q "GET" Get (pure (Nothing, []))
  <|> q "POST" Post (p_chunkedBody <|> p_body)
  where
    q :: Monad m
      => String
      -> Method
      -> ParsecT String Int m ((Maybe String, [Header]))
      -> ParsecT String Int m HttpRequest
    q name ctor body = do
      r <- req
      u <- url
      hs <- p_headers
      (mb, hs') <- body
      return $ HttpRequest r u (hs' ++ hs) mb
      -- HttpRequest <$> req <*> url <*> p_headers <*> body
      where
        req =
          ctor <$ string name <* char ' '

    url :: Monad m => ParsecT [Char] a m String
    url =
      optional (char '/')
      *> manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
      <* crlf


p_body :: Monad m => ParsecT String Int m ((Maybe String, [Header]))
p_body =
  liftBody (many anyChar) (pure [])


liftBody :: Applicative f => f a -> f b -> f (Maybe a, b)
liftBody x y =
  first Just <$> liftA2 (,) x y


-- data Chunk =
--   Chunk { csize :: Int
--         , cext :: [(String, Maybe String)]
--         , cdata :: String
--         }
--   deriving Show


p_chunkedBody :: Monad m => ParsecT String Int m ((Maybe String, [Header]))
p_chunkedBody =
  liftBody cdata cheaders
  where
    cdata =
      fmap concat (chunk `manyTill` lastChunk)

    cheaders =
      trailer <* crlf

    chunk :: Monad m => ParsecT String Int m String
    chunk = do
      x <- chunkSize
      _exts <- chunkExt
      _ <- crlf
      maybe failData readData (toSize x)
      where
        failData =
          parserFail "chunk size is not a valid hex number"

        readData n = do
          -- setState n -- OMG is this "shared state" among funcs!?
          d <- chunkData n
          _ <- crlf
          return d

        toSize x =
          case readHex x of
            [(n,[])] ->
              Just n

            _ ->
              Nothing

        chunkSize =
          many1 hexDigit

        chunkData :: Monad m => Int -> ParsecT String Int m String
        chunkData n =
          -- n <- getState
          count n anyChar

    chunkExt :: Monad m => ParsecT String Int m [(String, Maybe String)]
    chunkExt =
      ((:) <$> ext <*> many ext) <|> pure []
      where
        -- ext :: Monad m => ParsecT String Int m (String, Maybe String)
        ext =
          char ';' *> liftA2 (,) name (optionMaybe $ char '=' *> val)

        name =
          httpToken

        val =
          qdstring <|> httpToken

    lastChunk :: Monad m => ParsecT String Int m Char
    lastChunk =
      char '0' <* optional chunkExt <* crlf

    trailer :: Monad m => ParsecT String Int m [Header]
    trailer =
      p_headers


-- token          = 1*<any CHAR except CTLs or separators>
httpToken :: Monad m => ParsecT String Int m String
httpToken =
  many1 tchar
  where
    tchar =
      satisfy (not . (`elem` (separators ++ ctls)))


separators :: String
separators =
  "()<>@,;:\\/[]?={} \t"


ctls :: String
ctls =
  fmap chr [0..31]


-- quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
qdstring :: Monad m => ParsecT String Int m String
qdstring =
  between (char '"') (char '"') (qdtext <|> qdpair)
  where
    -- qdtext         = <any TEXT except <">>
    qdtext =
      anyChar `manyTill` choice noqdtext

    -- TEXT           = <any OCTET except CTLs, but including LWS>
    noqdtext =
      fmap char (['"', '\DEL'] ++ ctls)

    -- quoted-pair    = "\" CHAR
    qdpair =
      char '\\' *> liftA singleton anyChar
      where
        singleton x =
          [x]


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
