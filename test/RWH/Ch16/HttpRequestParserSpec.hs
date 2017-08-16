{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module RWH.Ch16.HttpRequestParserSpec (spec)

where

import Test.Hspec
import Test.QuickCheck
import Data.Char (isAlphaNum)
import Control.Applicative (liftA2)
import Control.Monad.Identity
import Data.Either (either, isLeft, isRight)
import RWH.Ch16.HttpRequestParser
import Text.Parsec
import Text.Parsec.Error
import System.IO.Unsafe (unsafePerformIO)


spec :: Spec
spec =
  describe "Http request parser" $ do
    describe "parse headers" $ do
      it "should fail if 'Content-Length' header is valid" $ do
        let req =
              "GET /foo HTTP/1.1\r\nContent-Length: 4096\r\n\r\n"
        parseHeaders req `shouldSatisfy` isRight

      it "should succeed if 'Content-Length' header is invalid" $ do
        let req =
              "GET /foo HTTP/1.1\r\nContent-Length: foobar\r\n\r\n"
        parseHeaders req `shouldSatisfy` isLeft

      it "should fail if header line is too long" $ property prop_dos


parseHeaders :: String -> Either ParseError HttpRequest
parseHeaders =
  runIdentity . runParserT (p_request :: ParsecT String Int Identity HttpRequest) 0 "HTTP GET"


prop_dos :: VeryLongString -> Bool
prop_dos (VeryLongString xs) =
  let req =
        "GET /foo HTTP/1.1\r\nIf-Match: " ++ xs ++ "\r\n\r\n"
      expectedErrorMessage =
        Message "Header line too big"
      eres = parseHeaders req
  in either (elem expectedErrorMessage . errorMessages) (const False) eres

newtype VeryLongString =
  VeryLongString String
  deriving Show


instance Arbitrary VeryLongString where
  arbitrary =
    -- length "If-Match: " == 10
    VeryLongString <$> vectorOf 4087 (arbitrary `suchThat` isAlphaNum)


instance Arbitrary Method where
  arbitrary =
    oneof (fmap pure [Get, Post])


instance Arbitrary HttpRequest where
  arbitrary =
    HttpRequest <$> arbitrary <*> arbitrary <*> pure [] <*> pure Nothing
