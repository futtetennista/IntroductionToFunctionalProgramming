{-# LANGUAGE OverloadedStrings #-}

module Calculator
where

import Prelude hiding (exp)
import Control.Applicative (Alternative (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C


data Expr
  = Const Int
  -- | Var a
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving (Eq, Show)


eval :: Expr -> Maybe Int
eval (Const x) =
  Just x
eval (Add x y) =
  (+) <$> eval x <*> eval y
eval (Sub x y) =
  (-) <$> eval x <*> eval y
eval (Mul x y) =
  (*) <$> eval x <*> eval y
eval (Div x y) = do
  y' <- eval y
  if y' == 0 then Nothing else do x' <- eval x ; return (x' `div` y')
eval (Exp x y) = do
  (^) <$> eval x <*> eval y


newtype Parser a =
  P { parse :: Text -> [(a, Text)] }


instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p =
    P (\xs -> case parse p xs of
                [] ->
                  []

                [(x, ys)] ->
                  [(f x, ys)])


instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\xs -> [(x, xs)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px =
    P (\xs -> case parse pf xs of
                [] ->
                  []
                [(f, ys)] ->
                  parse (fmap f px) ys)


instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f =
    P (\xs -> case parse pa xs of
                [] ->
                  []

                [(x, ys)] ->
                  parse (f x) ys)


instance Alternative Parser where
  -- empty :: Parser a
  empty =
    P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 =
    P (\xs -> case parse p1 xs of
                [] ->
                  parse p2 xs

                ys ->
                  ys)


item :: Parser Char
item =
  P item'
  where
    item' xs
      | T.null xs =
        []
      | otherwise =
          [(T.head xs, T.tail xs)]


mfilterp :: (Char -> Bool) -> Parser Char
mfilterp p =
  do x <- item ; if p x then return x else empty


digit :: Parser Char
digit =
  mfilterp C.isDigit


lower :: Parser Char
lower =
  mfilterp C.isLower


alphanum :: Parser Char
alphanum =
  mfilterp C.isAlphaNum


char :: Char -> Parser Char
char c =
  mfilterp (==c)


mvar :: Parser Text
mvar =
  do x <- lower ; xs <- many alphanum ; return $ T.pack (x:xs)


mstring :: Text -> Parser Text
mstring xs
  | T.null xs =
    return T.empty
  | otherwise =
    do x <- char (T.head xs) ; ys <- mstring (T.tail xs) ; return (x `T.cons` ys)


mtextlen :: Int -> Parser Text
mtextlen n =
  do xs <- sequence . replicate n $ item ; return (T.pack xs)


mnat :: Parser Int
mnat =
  do n <- some digit ; return (read n)


mint :: Parser Int
mint =
  mnegnat <|> mnat
  where
    mnegnat =
      do _ <- char '-' ; n <- mnat ; return (negate n)


space :: Parser ()
space =
  do _ <- many (char ' ') ; return ()


mtoken :: Parser a -> Parser a
mtoken p =
  do _ <- space ; x <- p ; _ <- space ; return x


var :: Parser Text
var =
  mtoken mvar


symbol :: Text -> Parser Text
symbol =
  mtoken . mstring


natural :: Parser Int
natural =
  mtoken mnat


integer :: Parser Int
integer =
  mtoken mint


list :: Parser a -> Parser [a]
list f = do
  _ <- mtoken (char '[')
  x <- nonemptyxs <|> emptyxs
  _ <- mtoken (char ']')
  return x
  where
    emptyxs =
      return []

    nonemptyxs = do
      x <- f
      xs <- many (do _ <- mtoken (char ',') ; f)
      return (x:xs)


-- Ex. 13.6
{--

Grammar:
--------
expr ::= term (+ exp | - exp | nil)
term ::= exp (* term | / term | nil)
exp  ::= fact (^ exp | nil)
fact ::= (expr) | int
int  ::= … | -1 | 0 | 1 | …

--}
expr :: Parser Expr
expr = do
  leftop <- term
  add leftop <|> return leftop
    where
      add x =
        do op <- parseOp ; y <- expr ; return (op x y)

      parseOp =
        (do _ <- symbol "+" ; return Add) <|> (do _ <- symbol "-" ; return Sub)


-- Ex. 13.8
-- New grammar for left-associative expressions:
-- expr ::= ((term + | term -) expr | nil) | term
-- expr ::= expr (+ term | - term | nil) | term
expr' :: Parser Expr
expr' =
  do leftop <- term ; exprleft leftop
  -- TODO: 4. (this impl isn't left-associative)
  -- do leftop <- term
  --    (do op <- parseOp
  --        es <- many expr'
  --        return (foldl op leftop es)) <|> return leftop
  where
    exprleft :: Expr -> Parser Expr
    exprleft x =
      (do op <- parseOp ; y <- term ; exprleft (op x y)) <|> return x

    parseOp :: Parser (Expr -> Expr -> Expr)
    parseOp =
      (do _ <- symbol "+" ; return Add) <|> (do _ <- symbol "-" ; return Sub)


term :: Parser Expr
term = do
  leftop <- exp
  mulexpr leftop <|> return leftop
  where
    mulexpr x =
      do op <- parseOp ; y <- term ; return (op x y)

    parseOp =
      (do _ <- symbol "*" ; return Mul) <|> (do _ <- symbol "/" ; return Div)


-- Ex. 13.7
exp :: Parser Expr
exp = do
  leftop <- factor
  expexpr leftop <|> return leftop
  where
    expexpr x =
      do _ <- symbol "^" ; y <- exp ; return (Exp x y)


factor :: Parser Expr
factor =
  parensexpr <|> constexpr
  where
    constexpr =
      do x <- integer ; return (Const x)

    parensexpr =
      do _ <- symbol "(" ; e <- expr ; _ <- symbol ")" ; return e


-- UI
writeat :: (Int, Int) -> Text -> IO ()
writeat p xs =
  do goto p ; putStr (T.unpack xs)
  where
    goto (x, y) =
      putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"


cls :: IO ()
cls =
  putStr "\ESC[2J"


box :: [Text]
box =
  [ "+---+---+---+---+"
  , "|               |"
  , "+---+---+---+---+"
  , "| q | c | d | = |"
  , "+---+---+---+---+"
  , "| 1 | 2 | 3 | + |"
  , "+---+---+---+---+"
  , "| 4 | 5 | 6 | - |"
  , "+---+---+---+---+"
  , "| 7 | 8 | 9 | * |"
  , "+---+---+---+---+"
  , "| 0 | ( | ) | / |"
  , "+---+---+---+---+"
  ]


buttons :: Text
buttons =
  standard `T.append` extra
  where
    standard =
      "qcd=123+456-789*0()/"

    extra =
      "QCD \ESC\BS\DEL\n"


showbox :: IO ()
showbox =
  sequence_ [writeat (1, y) b | (y, b) <- zip [1..] box]


displayWidth :: Int
displayWidth =
  13


type ErrorMsg =
  Text


display :: Text -> ErrorMsg -> IO ()
display xs msg = do
  writeat (1, 15) $ if T.null msg then eraseln else ("Error: " `mappend` msg)
  writeat (3, 2) (T.replicate displayWidth " ")
  writeat (3, 2) (T.reverse displayWidthChars)
  where
    displayWidthChars =
      T.take displayWidth (T.reverse xs)

    eraseln =
      "\ESC[2K"


calc :: Text -> ErrorMsg -> IO ()
calc xs msg = do
  display xs msg
  c <- getChar
  maybe (do beep ; calc xs T.empty) (process xs) (T.find (==c) buttons)
  where
    process :: Text -> Char -> IO ()
    process ys c
      | boolfind c "cC" =
        clear
      | boolfind c "\ESCqQ" =
        quit
      | boolfind c "\BS\DELdD" =
        deletelast
      | c == '\n' =
        case eval' ys of
          Left msg' ->
            calc ys msg'

          Right x ->
            calc (T.pack (show x)) T.empty
      | c == ' ' =
        calc ys T.empty
      | otherwise =
        press
      where
        quit =
          cls

        press =
          calc (xs `T.snoc` c) T.empty

        deletelast =
          calc (T.init ys) T.empty


    eval' :: Text -> Either Text Int
    eval' ys = do
      case parse expr ys of
        [] ->
          Left "invalid input"

        [(e, zs)] ->
          case T.null zs of
            True ->
              maybe (Left "division by zero") Right (eval e)

            False ->
              Left ("invalid input ^--- " `mappend` zs `mappend` " ---^")


    boolfind x ys =
      maybe False (const True) (T.find (==x) ys)

    beep :: IO ()
    beep =
      putStr "\BEL"



clear :: IO ()
clear =
  calc T.empty T.empty


run :: IO ()
run =
  do cls ; showbox ; clear
