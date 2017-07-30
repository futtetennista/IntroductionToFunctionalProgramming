{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module RWH.Ch8.GlobRegex ( globToRegex
                          , matchesGlob
                          , matchesGlob'
                          )
where

import Text.Regex.Posix ((=~))
import Data.Monoid ((<>))
import Data.Bits ((.|.))
import Control.Monad.State (State, put, runState)
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Text.Regex.Posix.ByteString as R
import qualified Data.Either as E (either)


type CaseSensitive =
  Bool


type Recursive =
  Bool


type Match =
  Bool


globToRegex :: B.ByteString -> (B.ByteString, Recursive)
globToRegex cs =
  first (\xs -> '^' `C8.cons` (xs `C8.snoc` '$')) statefulRegex
  where
    statefulRegex =
      runState (globToRegex' cs) False


globToRegex' :: B.ByteString  -> State Recursive B.ByteString
globToRegex' cs
  | B.null cs =
    return ""
  | B.take 2 cs == "**" =
    do put True ; globToRegex' cs'
  | headIs '*' cs =
    do xs <- globToRegex' cs' ; return $ ".*" <> xs
  | headIs '?' cs =
    do xs <- globToRegex' cs' ; return $ '.' `C8.cons` xs
  | B.take 2 cs == ("[!") =
    do xs <- charClass (B.tail cs') ; return $ "[^" <> (B.head cs' `B.cons` xs)
  | headIs '[' cs && not (B.null cs') =
    do xs <- charClass (B.tail cs') ; return $ '[' `C8.cons` B.head cs' `B.cons` xs
  | headIs '[' cs && B.null cs' =
    error "unterminated character class"
  | otherwise =
    do xs <- globToRegex' cs' ; return $ escape (C8.head cs) <> xs
  where
    cs' =
      B.tail cs


headIs :: Char -> B.ByteString -> Bool
headIs c cs =
  c == C8.head cs


charClass :: B.ByteString -> State Recursive B.ByteString
charClass cs
  | B.null cs =
    error "unterminated character class"
  | headIs ']' cs =
    do xs <- globToRegex' (B.tail cs) ; return $ ']' `C8.cons` xs
  | otherwise =
    do xs <- charClass (B.tail cs) ; return $ B.head cs `B.cons` xs


escape :: Char -> B.ByteString
escape c =
  maybe (C8.singleton c) (('\\' `C8.cons`) . C8.singleton) $ C8.find (==c) regexChars
  where
    regexChars =
      "\\+()^$.{}]|"


-- "(?i)foo" should be a valid POSIX regex makes the whole thing blow up with: `*** Exception: user error (Text.Regex.Posix.String died: (ReturnCode 13,"repetition-operator operand invalid"))` ?!
matchesGlob :: FilePath -> B.ByteString -> Bool
matchesGlob fileName pat =
  fst $ first (fileName =~) (globToRegex pat)


matchesGlob' :: FilePath -> B.ByteString -> CaseSensitive -> IO (Match, Recursive)
matchesGlob' fileName pat csFlg = do
  eCompRegex <- R.compile compOpt R.execBlank regex
  E.either (error . snd) execRegex eCompRegex
  where
    (regex, rec') =
      globToRegex pat

    compOpt =
      if csFlg
      then R.compExtended  .|. R.compNewline
      else R.compIgnoreCase .|. R.compExtended  .|. R.compNewline

    execRegex compRegex = do
      res <- R.regexec compRegex (C8.pack fileName)
      E.either (error . snd) (return . (, rec') . matches) res

    matches =
     maybe False (const True)
