module Chapter4( convert
               , convertWithFullStop
               , convertNatural
               , convertMoney
               , reverseConvert
               )
where


units :: [String]
units =
  [ "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]


teens :: [String]
teens =
  [ "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eigthteen"
  , "nineteen"
  ]


tens :: [String]
tens =
  [ "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]


convert2 :: Int -> String
convert2 x =
  combine2 $ x `divMod` 10


combine2 :: (Int, Int) -> String
combine2 (0, 0) =
  ""

combine2 (0, u) =
  units!!(u - 1)

combine2 (1, u) =
  teens!!u

combine2 (t, 0) =
  tens!!(t - 2)

combine2 (t, u) =
  tens!!(t - 2) ++ "-" ++ units!!(u - 1)


convert3 :: Int -> String
convert3 x =
  combine3 $ x `divMod` 100


combine3 :: (Int, Int) -> String
combine3 (0, t) =
  convert2 t

combine3 (h, 0) =
  units!!(h - 1) ++ " hundred"

combine3 (h, t) =
  units!!(h - 1) ++ " hundred and " ++ convert2 t


convert6 :: Int -> String
convert6 x =
  combine6 $ x `divMod` 1000


combine6 :: (Int, Int) -> String
combine6 (0, h) =
  convert3 h

combine6 (t, 0) =
  convert3 t ++ " thousand"

combine6 (t, h) =
  convert3 t ++ " thousand" ++ link ++ convert3 h
  where
    link =
      if h < 100 then " and " else " "


-- Ex. 4.1.1
convertWithFullStop :: Int -> String
convertWithFullStop x =
  convert6 x ++ "."


-- Ex. 4.1.2
convert :: Int -> String
convert =
  convert6


-- Ex. 4.1.3
convertNatural :: Int -> String
convertNatural x
  | x < 0 =
    "minus " ++ convertWithFullStop (abs x)
  | otherwise =
    convertWithFullStop x

-- Ex. 4.1.4
type Pound =
  Int

type Pence =
  Int

convertMoney :: Int -> String
convertMoney x =
  combineMoney $ x `divMod` 100


combineMoney :: (Pound, Pence) -> String
combineMoney (0, pence) =
  convert2 pence ++ " pence"

combineMoney (pound, pence) =
  convert pound ++ pluralise ++ link ++ convert2 pence ++ " pence"
  where
    pluralise =
      if pound >= 2 then " pounds" else " pound"

    link =
      if pence == 0 then "" else " and "


-- Ex. 4.1.5
reverseConvert :: String -> Int
reverseConvert str =
  toNum . reverseConvert' str $ (0, 0, 0)
  where
    toNum (t, h, d) =
      t + h + d

    reverseConvert' [] tuple =
      tuple

    reverseConvert' str' tuple@(t, h, d) =
      case span (/=' ') str' of
        ("thousand", rest) ->
          reverseConvert' rest (calculateThousand(h, d), 0, 0)

        ("hundred", rest) ->
          reverseConvert' rest (t, (d * 100), 0)

        ("and", rest) ->
          reverseConvert' rest tuple

        ("", rest) ->
          reverseConvert' (dropWhile (==' ') rest) tuple

        (s, rest) ->
          reverseConvert' rest (t, h, reverseTens s)

    calculateThousand (0, d) =
      d * 1000
    calculateThousand (h, 0) =
      h * 10
    calculateThousand (h, d) =
      (h + d) * 1000


reverseTens :: String -> Int
reverseTens str
  | elem '-' str =
    10 * (elemIndex prefix tens + 2) + (reverseUnits $ tail postfix)
  | otherwise =
    reverseTeens str
  where
    (prefix, postfix) =
      span (/= '-') str


reverseTeens :: String -> Int
reverseTeens str
  | elem str teens =
    10 + (elemIndex str teens)
  | otherwise =
    reverseUnits str


reverseUnits :: String -> Int
reverseUnits str =
  elemIndex str units + 1


elemIndex :: String -> [String] -> Int
elemIndex str xs =
  case map snd $ filter ((==str) . fst) (zip xs [0..length xs - 1]) of
    [x] ->
      x
    [] ->
      error $ "No index found for value " ++ str
    (_:_) ->
      error "Multiple indexes"
