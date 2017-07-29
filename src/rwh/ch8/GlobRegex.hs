{-# LANGUAGE OverloadedStrings #-}
module RWH.Ch8.GlobRegex ( globToRegex
                          , matchesGlob
                          , matchesGlob'
                          )
where

import Text.Regex.Posix ((=~))
import Data.Monoid ((<>))
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Text.Regex.Posix.ByteString as R
import qualified Data.Either as E (either)


globToRegex :: B.ByteString -> CaseSensitive -> B.ByteString
globToRegex cs b =
  '^' `C8.cons` (globToRegex' cs b `C8.snoc` '$')


globToRegex' :: B.ByteString -> CaseSensitive -> B.ByteString
globToRegex' cs b
  | B.null cs =
    ""
  | headIs '*' cs =
    ".*" <> globToRegex' cs' b
  | headIs '?' cs =
    '.' `C8.cons` globToRegex' cs' b
  | B.take 2 cs == ("[!") =
    "[^" <> (B.head cs' `B.cons` charClass (B.tail cs') b)
  | headIs '[' cs && not (B.null cs') =
    '[' `C8.cons` B.head cs' `B.cons` charClass (B.tail cs') b
  | headIs '[' cs && B.null cs' =
    error "unterminated character class"
  | otherwise =
    escape (C8.head cs) <> globToRegex' (B.tail cs) b
  where
    cs' =
      B.tail cs


headIs :: Char -> B.ByteString -> Bool
headIs c cs =
  c == C8.head cs


charClass :: B.ByteString -> CaseSensitive -> B.ByteString
charClass cs b
  | B.null cs =
    error "unterminated character class"
  | headIs ']' cs =
    ']' `C8.cons` globToRegex' (B.tail cs) b
  | otherwise =
    B.head cs `B.cons` charClass (B.tail cs) b


escape :: Char -> B.ByteString
escape c =
  maybe (C8.singleton c) (('\\' `C8.cons`) . C8.singleton) $ C8.find (==c) regexChars
  where
    regexChars =
      "\\+()^$.{}]|"


type CaseSensitive =
  Bool


-- "(?i)foo" should be a valid POSIX regex makes the whole thing blow up with: `*** Exception: user error (Text.Regex.Posix.String died: (ReturnCode 13,"repetition-operator operand invalid"))` ?!
matchesGlob :: FilePath -> B.ByteString -> Bool
matchesGlob fileName pat =
  fileName =~ globToRegex pat True


matchesGlob' :: FilePath -> B.ByteString -> CaseSensitive -> IO Bool
matchesGlob' fileName pat csFlg = do
  eCompRegex <- R.compile compOpt R.execBlank (globToRegex pat csFlg)
  E.either (error . snd) execRegex eCompRegex
  where
    compOpt =
      if csFlg
      then R.compExtended  .|. R.compNewline
      else R.compIgnoreCase .|. R.compExtended  .|. R.compNewline

    execRegex compRegex = do
      res <- R.regexec compRegex (C8.pack fileName)
      E.either (error . snd) (return . matches) res

    matches =
     maybe False (const True)
