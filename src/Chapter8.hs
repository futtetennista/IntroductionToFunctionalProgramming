module Chapter8
where


-- 8.2.3
data Employee
  = E { name :: String
      , sex :: Sex
      , birth :: Date
      , appointment :: Date
      }
    deriving Show

data Sex
  = Male
  | Female
  | Other
  deriving Show

data Date =
  D { year :: Int
    , month :: Int
    , day :: Int
    }
  deriving Show


served :: Date -> Int -> [Employee] -> [String]
served _today@(D y m _) duration =
  map name . filter xPlusMonths
  where
    xPlusMonths x =
      12 * (y - year (appointmentDate x)) - 1 + (12 - m) + (12 - month (appointmentDate x)) >= duration * 12


served20PlusYears :: Date -> [Employee] -> [String]
served20PlusYears today =
  served today 20


appointmentDate :: Employee -> Date
appointmentDate (E _ _ _ d) =
  d


birthDate :: Employee -> Date
birthDate (E _ _ b _) =
  b


yearsToRetirement :: Date -> Employee -> Int
yearsToRetirement _today@(D y _ _) x
  | appointmentBefore1980 =
    65 - age
  | otherwise =
    60 - age
  where
    appointmentBefore1980 =
      year (appointmentDate x) < 1980

    age =
      y - year (birthDate x)


-- Ex. 8.2.5
data Temperature
  = Celsius Int
  | Fahrenheit Int
  | Kelvin Int
  deriving Show


-- Is there a more concise way to implement this?
instance Eq Temperature where
  (Celsius x) == (Celsius y) =
    x == y
  (Fahrenheit x) == (Fahrenheit y) =
    x == y
  (Kelvin x) == (Kelvin y) =
    x == y
  ct@(Celsius _) == ft@(Fahrenheit _) =
    ct == fst (convert ft)
  ct@(Celsius _) == kt@(Kelvin _) =
    ct == snd (convert kt)
  ft@(Fahrenheit _) == ct@(Celsius _) =
    ft == snd (convert ct)
  ft@(Fahrenheit _) == kt@(Kelvin _) =
    ft == snd (convert kt)
  kt@(Kelvin _) == ct@(Celsius _) =
    kt == fst (convert ct)
  kt@(Kelvin _) == ft@(Fahrenheit _) =
    kt == snd (convert ft)


-- Is there a more concise way to implement this?
instance Ord Temperature where
  (Celsius x) <= (Celsius y) =
    x <= y
  (Fahrenheit x) <= (Fahrenheit y) =
    x <= y
  (Kelvin x) <= (Kelvin y) =
    x <= y
  ct@(Celsius _) <= ft@(Fahrenheit _) =
    ct <= fst (convert ft)
  ct@(Celsius _) <= kt@(Kelvin _) =
    ct <= snd (convert kt)
  ft@(Fahrenheit _) <= ct@(Celsius _) =
    ft <= snd (convert ct)
  ft@(Fahrenheit _) <= kt@(Kelvin _) =
    ft <= snd (convert kt)
  kt@(Kelvin _) <= ct@(Celsius _) =
    kt <= fst (convert ct)
  kt@(Kelvin _) <= ft@(Fahrenheit _) =
    kt <= snd (convert ft)


-- this API is sub-optimal since the caller needs to know the order in which the conversion appear in a pair. What's a better alternative?
convert :: Temperature -> (Temperature, Temperature)
convert (Celsius x) =
  (Fahrenheit ((x + 32) * 5 `div` 9), Kelvin z)
  where
    z =
      floor (fromIntegral x + 273.15 :: Double)
convert (Fahrenheit x) =
  (Celsius ((x * 9) `div` 5 - 32), Kelvin y)
  where
    y =
      floor ((fromIntegral x + 459.67 :: Double) * 0.5555555555555556 :: Double)
convert (Kelvin x) =
  (Celsius z, Fahrenheit y)
  where
    z =
      floor (fromIntegral x - 273.15 :: Double)

    y =
      floor (1.8 * (fromIntegral x - 273.15 :: Double) + 32.0 :: Double)

-- maybe like this ?!
data Tag
  = Celsius'
  | Fahrenheit'
  | Kelvin'
  deriving Show


convert' :: Temperature -> Tag -> Temperature
convert' ct@(Celsius _) Celsius' =
  ct
convert' (Celsius x) Fahrenheit' =
  Fahrenheit ((x + 32) * 5 `div` 9)
convert' (Celsius x) Kelvin' =
  Kelvin z
  where
    z =
      floor (fromIntegral x + 273.15 :: Double)
convert' ft@(Fahrenheit _) Fahrenheit' =
  ft
convert' (Fahrenheit x) Celsius' =
  Celsius ((x * 9) `div` 5 - 32)
convert' (Fahrenheit x) Kelvin' =
  Kelvin y
  where
    y =
      floor ((fromIntegral x + 459.67 :: Double) * 0.5555555555555556 :: Double)
convert' kt@(Kelvin _) Kelvin' =
  kt
convert' (Kelvin x) Celsius' =
  Celsius z
  where
    z =
      floor (fromIntegral x - 273.15 :: Double)
convert' (Kelvin x) Fahrenheit' =
  Fahrenheit y
  where
    y =
      floor (1.8 * (fromIntegral x - 273.15 :: Double) + 32.0 :: Double)


-- Alternatively
type Temperature' =
  (Tag, Int)


convert'' :: Temperature' -> Tag -> Temperature'
convert'' _temp _tag =
  undefined
