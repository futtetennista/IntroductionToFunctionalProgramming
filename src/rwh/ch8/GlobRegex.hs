{-# LANGUAGE TupleSections, OverloadedStrings #-}
module RWH.Ch8.GlobRegex ( ErrorMsg
                         , globToRegex
                         , matchesGlob
                         , matchesGlob'
                         )
where

import Text.Regex.Posix ((=~))
import Data.Monoid ((<>))
import Data.Bits ((.|.))
import Control.Monad.State (StateT, put, runStateT)
import Control.Monad.Except
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Either as E (either)
import qualified Text.Regex.Posix.ByteString as R


type ErrorMsg =
  String


type CaseSensitive =
  Bool


type Recursive =
  Bool


type Match =
  Bool


type Result =
  StateT Recursive (Except String)


globToRegex :: B.ByteString -> Either ErrorMsg (B.ByteString, Recursive)
globToRegex cs =
  E.either Left (Right . first (('^' `C8.cons`) . (`C8.snoc` '$'))) eRegex
  where
    eRegex :: Either ErrorMsg (B.ByteString, Recursive)
    eRegex =
      runExcept (runStateT (globToRegex' cs) False)


globToRegex' :: B.ByteString -> Result B.ByteString
globToRegex' cs
  | B.null cs =
    return ""
  | B.take 2 cs == "**" =
    do put True ; globToRegex' cs'
  | headEqualTo '*' cs =
    (".*" <>) <$> globToRegex' cs'
  | headEqualTo '?' cs =
    ('.' `C8.cons`) <$> globToRegex' cs'
  | B.take 2 cs == ("[!") =
    let cs' =
          B.drop 2 cs
    in (("[^" <>) . (B.head cs' `B.cons`)) <$> charClass (B.tail cs')
  | headEqualTo '[' cs && not (B.null cs') =
    (('[' `C8.cons`) . (B.head cs' `B.cons`)) <$> charClass (B.tail cs')
  | headEqualTo '[' cs && B.null cs' =
    throwError "unterminated character class"
  | otherwise =
    (escape (C8.head cs) <>) <$> globToRegex' cs'
  where
    cs' =
      B.tail cs


headEqualTo :: Char -> B.ByteString -> Bool
headEqualTo c cs =
  c == C8.head cs


charClass :: B.ByteString -> Result B.ByteString
charClass cs
  | B.null cs =
    throwError "unterminated character class"
  | headEqualTo ']' cs =
    (']' `C8.cons`) <$> globToRegex' (B.tail cs)
  | otherwise =
    (B.head cs `B.cons`) <$> charClass (B.tail cs)


escape :: Char -> B.ByteString
escape c =
  maybe (C8.singleton c) (('\\' `C8.cons`) . C8.singleton) $ C8.find (==c) regexChars
  where
    regexChars =
      "\\+()^$.{}]|"


-- "(?i)foo" should be a valid POSIX regex makes the whole thing blow up with: `*** Exception: user error (Text.Regex.Posix.String died: (ReturnCode 13,"repetition-operator operand invalid"))` ?!
matchesGlob :: FilePath -> B.ByteString -> Either ErrorMsg Bool
matchesGlob fileName pat =
  E.either Left (Right . fst . first (fileName =~)) (globToRegex pat)


matchesGlob' :: FilePath -> B.ByteString -> CaseSensitive
             -> IO (Either ErrorMsg (Match, Recursive))
matchesGlob' fileName pat csFlg =
  case globToRegex pat of
    Left err ->
      return (Left err)

    Right (regex, rec') ->
      E.either returnErrorAsString (execRegex rec') =<< compileRegex regex
  where
    compileRegex =
      R.compile compOpt R.execBlank

    returnErrorAsString =
      return . Left . show . snd

    compOpt =
      if csFlg
      then R.compExtended  .|. R.compNewline
      else R.compExtended  .|. R.compNewline .|. R.compIgnoreCase

    execRegex rec' compRegex =
      let rightF =
            Right . (, rec') . matches
          leftF =
            Left . show . snd
          matches =
            maybe False (const True)
      in
        return . E.either leftF rightF =<< R.regexec compRegex (C8.pack fileName)
