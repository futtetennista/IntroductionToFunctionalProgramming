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
  runIdentity . runParserT p 0 "HTTP GET"
  where
    p :: ParsecT String Int Identity HttpRequest
    p =
      p_request


prop_dos :: VeryLongString -> Bool
prop_dos (VeryLongString xs) =
  let req =
        "GET /foo HTTP/1.1\r\nIf-Match: " ++ xs ++ "\r\n\r\n"

      expectedErrorMessage =
        Message "Header line too big"
  in either (elem expectedErrorMessage . errorMessages) (const False) (parseHeaders req)


newtype VeryLongString =
  VeryLongString String
  deriving Show


instance Arbitrary VeryLongString where
  arbitrary =
    -- length "If-Match: " == 10
    VeryLongString <$> vectorOf 4087 (arbitrary `suchThat` isAlphaNum)


-- instance Arbitrary Method where
--   arbitrary =
--     oneof (fmap pure [Get, Post])


-- instance Arbitrary HttpRequest where
--   arbitrary =
--     HttpRequest <$> arbitrary <*> arbitrary <*> pure [] <*> pure Nothing
