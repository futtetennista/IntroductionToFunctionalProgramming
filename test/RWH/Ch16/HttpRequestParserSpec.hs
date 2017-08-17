{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module RWH.Ch16.HttpRequestParserSpec (spec)

where

import Test.Hspec
import Test.QuickCheck
import Data.Char (isAlphaNum)
import Control.Applicative (liftA2)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Either (either, isLeft, isRight)
import RWH.Ch16.HttpRequestParser
import Text.Parsec (ParsecT, runParserT)
import Text.Parsec.Error (Message(..), ParseError, errorMessages)


spec :: Spec
spec =
  describe "Http request parser" $ do

    describe "parse headers" $ do
      it "should parse all request headers" $ do
        let req =
              "GET /foo HTTP/1.1\r\nMedia-Type: text/plain\r\nHost: 0.0.0.0\r\n\r\n"

            expectedReq =
              HttpRequest Get "foo" [("Media-Type", "text/plain"), ("Host", "0.0.0.0")] Nothing
        parseHttpRequest req `shouldBe` Right expectedReq

      it "should fail if 'Content-Length' header is valid" $ do
        let req =
              "GET /foo HTTP/1.1\r\nContent-Length: 4096\r\n\r\n"
        parseHttpRequest req `shouldSatisfy` isRight

      it "should succeed if 'Content-Length' header is invalid" $ do
        let req =
              "GET /foo HTTP/1.1\r\nContent-Length: foobar\r\n\r\n"
        parseHttpRequest req `shouldSatisfy` isLeft

      it "should fail if header is too long" $ property prop_dos_header

      it "should succeed if header is not too long" $ property prop_valid_header

    describe "parse no transfer encoding" $ do
      it "should parse simple body" $ do
        let req =
              "POST /foo HTTP/1.1\r\nMedia-Type: text/plain\r\nContent-Length: 12\r\n\r\nHello there!"

            expectedReq =
              HttpRequest Post "foo" [("Media-Type", "text/plain"), ("Content-Length", "12")] (Just "Hello there!")

        parseHttpRequest req `shouldBe` Right expectedReq

      it "should parse empty body" $ do
        let req =
              "POST /foo HTTP/1.1\r\nMedia-Type: text/plain\r\nContent-Length: 0\r\n\r\n"

            expectedReq =
              HttpRequest Post "foo" [("Media-Type", "text/plain"), ("Content-Length", "0")] (Just "")

        parseHttpRequest req `shouldBe` Right expectedReq


    describe "parse chunked transfer encoding" $ do
      it "should parse chunked body without entity headers" $ do
        let req =
              "POST /foo HTTP/1.1\r\nMedia-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
              ++ "6\r\nHello \r\n" ++ "6\r\nthere!\r\n" ++ "0\r\n\r\n"

            expectedReq =
              HttpRequest Post "foo" [("Media-Type", "text/plain"), ("Transfer-Encoding", "chunked")] (Just "Hello there!")

        parseHttpRequest req `shouldBe` Right expectedReq

      it "should parse chunked body with entity headers" $ do
        let req =
              "POST /foo HTTP/1.1\r\nMedia-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
              ++ "6\r\nHello \r\n" ++ "6\r\nthere!\r\n" ++ "0\r\nFoo: bar\r\n\r\n"

            expectedReq =
              HttpRequest Post "foo" [("Foo", "bar"), ("Media-Type", "text/plain"), ("Transfer-Encoding", "chunked")] (Just "Hello there!")

        parseHttpRequest req `shouldBe` Right expectedReq



parseHttpRequest :: String -> Either ParseError HttpRequest
parseHttpRequest =
  runIdentity . runParserT p 0 "HTTP request"
  where
    p :: ParsecT String Int Identity HttpRequest
    p =
      p_request


prop_dos_header :: VeryLongString -> Bool
prop_dos_header (VeryLongString xs) =
  let req =
        "GET /foo HTTP/1.1\r\nIf-Match: " ++ xs ++ "\r\n\r\n"

      expectedErrorMessage =
        Message "Header line too big"
  in either (elem expectedErrorMessage . errorMessages) (const False) (parseHttpRequest req)


prop_valid_header :: LongString -> Bool
prop_valid_header (LongString xs) =
  let req =
        "GET /foo HTTP/1.1\r\nIf-Match: " ++ xs ++ "\r\n\r\n"
  in either (const False) (const True) (parseHttpRequest req)


newtype VeryLongString =
  VeryLongString String
  deriving Show


instance Arbitrary VeryLongString where
  arbitrary =
    -- length "If-Match: " == 10
    VeryLongString <$> vectorOf 4087 (arbitrary `suchThat` isAlphaNum)


newtype LongString =
  LongString String
  deriving Show


instance Arbitrary LongString where
  arbitrary =
    -- length "If-Match: " == 10 (just within the allowed limit)
    LongString <$> vectorOf 4086 (arbitrary `suchThat` isAlphaNum)


-- instance Arbitrary Method where
--   arbitrary =
--     oneof (fmap pure [Get, Post])


-- instance Arbitrary HttpRequest where
--   arbitrary =
--     HttpRequest <$> arbitrary <*> arbitrary <*> pure [] <*> pure Nothing
