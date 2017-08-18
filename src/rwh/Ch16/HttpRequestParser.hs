module RWH.Ch16.HttpRequestParser ( HttpRequest(..)
                                  , Method(..)
                                  , Header
                                  , p_request
                                  --, p_query
                                  )
where


import Data.Char (chr, toLower)
import Control.Applicative (liftA, liftA2, liftA3)
import Control.Arrow (first)
import Control.Exception (bracket, catch, finally)
import Text.Parsec
import Numeric
import Network
import System.IO
import System.Timeout (timeout)
import Control.Concurrent.Async


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
{-
could `try` be a performance bottleneck? If yes, this can refactored to smth like:
q name ctor body = do
  ...
  where chooseBodyParser headers = ...

and probably use `Map String String` instead of [Header]
-}
  q "GET" Get (\_ -> pure (Nothing, []))
  <|> q "POST" Post (\eol -> try p_chunkedBody <|> p_body eol)
  where
    q :: Monad m
      => String
      -> Method
      -> (ParsecT String Int m () -> ParsecT String Int m ((Maybe String, [Header])))
      -> ParsecT String Int m HttpRequest
    q name ctor body = do
      r <- req
      u <- url
      hs <- p_headers
      (mb, hs') <- body (mediaTypeEol hs)
      return $ HttpRequest r u (hs' ++ hs) mb
      where
        req =
          ctor <$ string name <* char ' '

    -- TODO
    mediaTypeEol :: Monad m => [Header] -> ParsecT String Int m ()
    mediaTypeEol _ =
      eof

    url :: Monad m => ParsecT [Char] a m String
    url =
      optional (char '/')
      *> notEOL `manyTill` (try $ string " HTTP/1." <* oneOf "01")
      <* crlf


p_body :: Monad m
       => ParsecT String Int m ()
       -> ParsecT String Int m (Maybe String, [Header])
p_body p_eof =
  mapLiftBody (anyChar `manyTill` p_eof) (pure [])


mapLiftBody :: Applicative f => f a -> f b -> f (Maybe a, b)
mapLiftBody x y =
  first Just <$> liftA2 (,) x y


-- data Chunk =
--   Chunk { csize :: Int
--         , cext :: [(String, Maybe String)]
--         , cdata :: String
--         }
--   deriving Show


p_chunkedBody :: Monad m => ParsecT String Int m (Maybe String, [Header])
p_chunkedBody =
  mapLiftBody cdata p_headers
  where
    cdata =
      fmap concat (chunk `manyTill` lastChunk)

    chunk :: Monad m => ParsecT String Int m String
    chunk = do
      x <- chunkSize
      _exts <- chunkExt
      _ <- crlf
      maybe failData chunkData (toSize x)
      where
        failData =
          parserFail "chunk size is not a valid hex number"

        toSize x =
          case readHex x of
            [(n, [])] ->
              Just n

            _ ->
              Nothing

        chunkSize =
          many1 hexDigit

        chunkData :: Monad m => Int -> ParsecT String Int m String
        chunkData size =
          -- setState n -- OMG is this "shared state" among funcs!?
          -- n <- getState
          count size anyChar <* crlf

    chunkExt :: Monad m => ParsecT String Int m [(String, Maybe String)]
    chunkExt =
      exts <|> pure []
      where
        exts =
          (:) <$> ext <*> many ext

        -- ext :: Monad m => ParsecT String Int m (String, Maybe String)
        ext =
          char ';' *> liftA2 (,) name (optionMaybe $ char '=' *> val)

        name =
          httpToken (id :: ParsecT String Int m Char -> ParsecT String Int m Char)

        val =
          qdstring <|> httpToken (id :: ParsecT String Int m Char -> ParsecT String Int m Char)

    lastChunk :: Monad m => ParsecT String Int m ()
    lastChunk =
      () <$ char '0' <* chunkExt <* crlf


-- token = 1*<any CHAR except CTLs or separators>
httpToken :: Monad m
          => (ParsecT String Int m Char -> ParsecT String Int m Char)
          -> ParsecT String Int m [Char]
httpToken p =
  -- (:) <$> statefulParser letter <*> (many . statefulParser) (letter <|> digit <|> oneOf "-_")
  many1 (p tchar)
  where
    -- tchar :: Monad m => ParsecT String Int m String -> ParsecT String Int m String
    tchar =
      noneOf (separators ++ ctls)
      -- satisfy (not . (`elem` (separators ++ ctls)))


separators :: String
separators =
  "()<>@,;:\\/[]?={} \t"


ctls :: String
ctls =
  fmap chr [0..31]


lws :: Monad m => ParsecT String Int m ()
lws =
  () <$ optional crlf <* many1 (oneOf " \t")


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
      httpToken statefulParser

    fieldContents :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    fieldContents contentParser =
      (many . statefulParser) space *> (contents . statefulParser) contentParser

    contents :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    contents p =
      liftA2 (++) (many1 p <* crlf) (continuation p <|> pure [])

    continuation :: Monad m => ParsecT String Int m Char -> ParsecT String Int m String
    continuation p =
      liftA2 (:) (' ' <$ many1 ((statefulParser . oneOf) " \t")) (contents p)

    statefulParser :: Monad m => ParsecT s Int m a -> ParsecT s Int m a
    statefulParser p = do
      xs <- boundedParser
      modifyState (+1)
      return xs
        where
          boundedParser = do
            headerLength <- getState
            if headerLength < 4096 then p else parserFail "Header line too big"


parseReq :: SourceName -> String -> Either ParseError HttpRequest
parseReq =
  runParser p_request 0


data HttpResponse =
  HttpResponse { statusCode :: Int
               , reasonPhrase :: String
               , respHeaders :: [Header]
               , respBody :: Maybe String
               }


instance Show HttpResponse where
  show x =
    statusLine ++ showHeaders ++ "\r\n" ++ showBody
    where
      statusLine =
        "HTTP/1.1 " ++ (show . statusCode) x ++ " " ++ reasonPhrase x

      showHeaders =
        concat . map (\(k, v) -> k ++ ": " ++ v ++ "\r\n") $ respHeaders x

      showBody =
        maybe "" id (respBody x)


emptyHttpResponse :: HttpResponse
emptyHttpResponse =
  HttpResponse 200 "Success" [] Nothing


main :: IO ()
main = do
  bracket open sClose loop
  where
    open =
      listenOn (PortNumber 8888)

    loop :: Socket -> IO()
    loop s = do
      withAsync (accept s) asyncHandleCon
      loop s

    asyncHandleCon a =
      catch (handleCon =<< wait a) handleEx
      where
        handleEx :: IOError -> IO ()
        handleEx e = do
          putStrLn (show e)
          return ()

    handleCon (h, hostname, port) =
      finally (handleReq secs30) (hClose h)
      where
        secs30 =
          5 * 1000 * 1000

        name =
          "socket(" ++ hostname ++ ":" ++ show port ++ ")"

        handleReq userTimeout = do
          req <- hGetContents h
          mparse <- parseHttpRequest req userTimeout
          maybe dropReq reply mparse

        dropReq =
          hPutStrLn h $ show emptyHttpResponse { statusCode = 408
                                               , reasonPhrase = "Request Timeout"
                                               }

        reply :: Either ParseError HttpRequest -> IO ()
        reply (Right httpRequest) =
          hPutStrLn h $ show emptyHttpResponse { respBody = Just ("I see you:\n" ++ show httpRequest) }
        reply (Left err) =
          hPutStrLn h $ show emptyHttpResponse { statusCode = 500
                                               , reasonPhrase = "Internal Server Error"
                                               , respBody = Just (show err)
                                               }

        parseHttpRequest req uTimeout =
          -- why inserting a `putStrLn (show req)` blocks ?!
          timeout uTimeout (return (parseReq name req))
