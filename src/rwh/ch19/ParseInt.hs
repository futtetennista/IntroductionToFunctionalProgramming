{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RWH.Ch19.ParseInt
where

import qualified Data.Char as C
import qualified Data.Bits as Bits
import qualified Control.Applicative as A
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F


data ParseError
  = NumericOverflow
  | EndOfInput
  | Chatty String
  deriving (Eq, Ord, Show)


newtype Parser a =
  P { runP :: E.ExceptT ParseError (S.State B.ByteString) a }
  deriving (Functor, Applicative, Monad, E.MonadError ParseError)


newtype Parser' a =
  P' { runP' :: S.StateT B.ByteString (E.Except ParseError) a }
  deriving (Functor, Applicative, Monad, E.MonadError ParseError)


instance A.Alternative Parser where
  -- empty :: Parser a
  empty =
    P $ E.throwError (Chatty "empty")

  -- (<*>) :: Parser a -> Parser a -> Parser a
  px <|> py =
    E.catchError px (const py)


-- class (S.MonadState s m) => ParserState m s a where
--   lift :: m s -> p a

-- instance ParserState Parser where
--   lift =
--     undefined

-- instance ParserState Parser' where
--   lift =
--     undefined


-- state monad not exposed -> we want to get a hold of it somehow though
liftP :: S.State B.ByteString a -> Parser a
liftP m =
  P (S.lift m)


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP S.get
  case B.uncons s of
    Nothing ->
      E.throwError EndOfInput

    Just (c, s')
      | p c ->
        liftP (S.put s') >> return c
      | otherwise ->
        E.throwError (Chatty $ "invalid character: '" ++ B.unpack s ++ "'")


runParser :: Parser a -> B.ByteString
          -> Either ParseError (a, B.ByteString)
runParser p xs =
  case S.runState (E.runExceptT . runP $ p) xs of
    (Left err, _) ->
      Left err

    (Right x, ys) ->
      Right (x, ys)


runParser' :: Parser' a -> B.ByteString
           -> Either ParseError (a, B.ByteString)
runParser' p =
  E.runExcept . S.runStateT (runP' p)


many :: Parser a -> Parser [a]
many p = do
  c <- p
  st <- liftP S.get
  if B.null st then return [c] else do (:) <$> pure c <*> many p


-- ƛ: runParser int "-9223372036854775808" :: Right (-9223372036854775808,"")
-- ƛ: runParser int "-9223372036854775809" :: Left EndOfInput ==> WHY !?
-- ƛ: runParser int "9223372036854775808"  :: Left NumericOverflow
-- ƛ: runParser int "9223372036854775807"  :: Right (9223372036854775807,"")
int :: Parser Int
int =
  (satisfy (=='-') >> digits (-)) A.<|> digits (+)
  where
    digits f =
      many (satisfy C.isDigit) >>= returnOrThrow . toInt f

    returnOrThrow Nothing =
      E.throwError NumericOverflow
    returnOrThrow (Just x) =
      return x


toInt :: (Int -> Int -> Int) -> [Char] -> Maybe Int
toInt f =
  F.foldlM (safeBuildInt f) 0 . fmap fromAscii


fromAscii :: Char -> Int
fromAscii =
  (flip (-) 48) . C.ord


safeBuildInt :: (Int -> Int -> Int) -> Int -> Int -> Maybe Int
safeBuildInt f 0 x =
  Just (0 `f` x)
safeBuildInt f acc x =
  if sign newAcc == sign acc then Just newAcc else Nothing -- overflow detected
  where
    newAcc =
      (acc * 10) `f` x


-- "Right and left shifts by amounts greater than or equal to the width of the type result in either zero or -1, depending on the sign of the value being shifted" [Haskell2010 Report 18.1]
sign :: Int -> Int
sign =
  flip Bits.shiftR 64
