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


-- Bogus
instance Monoid Doc where
  --mempty :: Doc
  mempty =
    Empty

  -- mappend  :: Doc -> Doc -> Doc
  Empty `mappend` y =
    y
  x `mappend` Empty =
    x
  x `mappend` y =
    Concat x y


char :: Char -> Doc
char =
  Char


text :: String -> Doc
text =
  Text


line :: Doc
line =
  Line


fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f =
  foldr f mempty
