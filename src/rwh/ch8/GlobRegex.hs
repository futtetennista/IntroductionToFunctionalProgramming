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
import Data.Char (isUpper, toLower, toUpper)
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
    put True >> globToRegex' cs'
  | headEqualTo '*' cs =
    (".*" <>) <$> globToRegex' cs'
  | headEqualTo '?' cs =
    ('.' `C8.cons`) <$> globToRegex' cs'
  | B.take 2 cs == ("[!") =
    let cs' =
          B.drop 2 cs
    in
      (("[^" <>) . (B.head cs' `B.cons`)) <$> charClass (B.tail cs')
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
matchesGlobRegex :: FilePath -> B.ByteString -> CaseSensitive -> Either ErrorMsg (Bool, Recursive)
matchesGlobRegex fileName pat _ =
  E.either Left (Right . first (fileName =~)) (globToRegex pat)


matchesGlob' :: FilePath -> B.ByteString -> CaseSensitive
             -> IO (Either ErrorMsg (Match, Recursive))
matchesGlob' fileName pat csFlg =
  case globToRegex pat of
    Left err ->
      return (Left err)

    Right (regex, rec') ->
      E.either failure (execRegex rec') =<< compileRegex regex
  where
    compileRegex =
      R.compile compOpt R.execBlank

    failure =
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


-- NATIVE MATCHER
data Rule
  = Str B.ByteString
  | Anything
  | AnySingleChar
  | CharClass Bool B.ByteString
  deriving Show


type GlobPat =
  [Rule]


runToGlobPat :: B.ByteString -> Either ErrorMsg (GlobPat, Recursive)
runToGlobPat xs =
  runExcept (runStateT (toGlobPat xs) False)


toGlobPat :: B.ByteString -> Result GlobPat
toGlobPat xs
  | C8.null xs =
    return []
  | C8.take 2 xs == "**" =
    put True >> toGlobPat (C8.drop 2 xs)
  | C8.head xs == '*' =
    (Anything:) <$> toGlobPat (C8.tail xs)
  | C8.head xs == '?' =
    (AnySingleChar:) <$> toGlobPat (C8.tail xs)
  | C8.take 2 xs == "[!" && (not . C8.null) (C8.tail xs) = do
      (ccs, xs') <- toCharClass (C8.drop (2 :: Int) xs)
      (CharClass False ccs:) <$> toGlobPat xs'
  | C8.head xs == '[' = do
      (ccs, xs') <- toCharClass (C8.tail xs)
      (CharClass True ccs:) <$> toGlobPat xs'
  | otherwise =
    do (str, xs') <- toChar xs ; (Str str:) <$> toGlobPat xs'


toCharClass :: B.ByteString -> Result (B.ByteString, B.ByteString)
toCharClass xs
  | C8.head xs == ']' =
    throwError "unterminated character class"
  | otherwise =
    toCharClassHelp (B.empty, xs)
  where
    toCharClassHelp (xs', ys)
      | C8.null ys =
        throwError "unterminated character class"
      | C8.head ys == ']' =
        return (xs', C8.tail ys)
      | otherwise =
        toCharClassHelp (xs' `C8.snoc` C8.head ys, C8.tail ys)


toChar :: B.ByteString -> Result (B.ByteString, B.ByteString)
toChar xs =
  return $ toCharHelp (B.empty, xs)
  where
    toCharHelp (ys, zs)
      | C8.null zs =
        (ys, zs)
      | isPatternChar (C8.head zs) =
        (ys, zs)
      | otherwise =
        toCharHelp (ys `C8.snoc` C8.head zs, C8.tail zs)

    isPatternChar :: Char -> Bool
    isPatternChar =
      (`elem` ("[*?" :: String))


matchesGlobNative :: FilePath -> B.ByteString -> CaseSensitive -> Either ErrorMsg (Bool, Recursive)
matchesGlobNative [] _ _ =
  Left "Empty file path"
matchesGlobNative xs pat csFlg =
  -- I didn't think about this before: even if `runToGlobPat` fails, the pattern matching will *not* fail because of how >>= works. Interesting.
  do (pat', rec') <- runToGlobPat pat ; return $ (C8.pack xs `matches` pat', rec')
  where
    matches xs' [] =
      C8.null xs'
    matches _ (Anything:_) =
      True
    matches xs' (AnySingleChar:rs) =
      (not . C8.null) xs' && matches (C8.tail xs') rs
    matches xs' (Str str:rs) =
      C8.take (C8.length str) xs' `caseAwareEq` str && matches (C8.drop (C8.length str) xs') rs
    matches xs' (CharClass flg bs:rs) =
      matchesCharClass (C8.head xs') bs && matches (C8.tail xs') rs
      where
        matchesCharClass c =
          let f = if flg then id else not
          in f . C8.any (==c) . C8.map (\x -> if isUpper x then toUpper x else toLower x)

    caseAwareEq zs ys =
      if csFlg then zs == ys else C8.map toLower zs == C8.map toLower ys



matchesGlob :: FilePath -> B.ByteString -> CaseSensitive -> Either ErrorMsg (Bool, Recursive)
matchesGlob =
  matchesGlobNative
