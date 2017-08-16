{-# LANGUAGE OverloadedStrings #-}
module RWH.Ch16.Csv
where

import Text.Parsec
import Text.Parsec.Prim (Parsec)
import Control.Exception (catch)
import qualified Data.ByteString.Char8 as C8


csvFile :: Parsec String a [[String]]
csvFile =
  line `endBy` eol


line :: Parsec String a [String]
line =
  cell `sepBy` (char ',')


cell :: Parsec String a String
cell =
  quotedCell <|> many (noneOf ",\r\n")


quotedCell :: Parsec String a String
quotedCell = do
  _ <- char '"'
  content <- many quotedChar
  _ <- char '"' <?> "quote at end of cell"
  return content


quotedChar :: Parsec String a Char
quotedChar =
  noneOf "\"" <|> try (string "\"\"" >> return '"')


eol :: Parsec String a String
eol =
  try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"


-- Is there a better way to use Parsec with bytestrings?
bytestring :: (String -> Parsec String a String)
           -> String
           -> Parsec String a C8.ByteString
bytestring f xs =
  C8.pack <$> f xs


parseCSV :: String -> Either ParseError [[String]]
parseCSV =
  parse csvFile "(unknown)"


main :: IO ()
main =
  catch readCsv (const $ putStrLn "File does not exist" >> main :: IOError -> IO ())
  where
    readCsv = do
      c <- readFile =<< getLine
      case parseCSV c of
        Left e ->
          putStrLn "Error parsing input:" >> print e

        Right r ->
          mapM_ print r
