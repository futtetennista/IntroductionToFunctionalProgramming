module RWH.Ch11.Prettify2
where


data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)


empty :: Doc
empty =
  Empty


char :: Char -> Doc
char =
  Char


text :: String -> Doc
text =
  Text


line :: Doc
line =
  Line

(<>)  :: Doc -> Doc -> Doc
Empty <> y =
  y
x <> Empty =
  x
x <> y =
  Concat x y


fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f =
  foldr f empty
