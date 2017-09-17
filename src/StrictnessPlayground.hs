-- https://www.fpcomplete.com/blog/2017/09/all-about-strictness
{-# LANGUAGE BangPatterns #-}

{-
Run with:
stack --resolver lts-8.6 ghc
--package conduit-combinators
--package deepseq
-- StrictnessPlayground.hs -O2

./StrictnessPlayground +RTS -s
-}

import Debug.Trace
import Control.DeepSeq (NFData, rnf, deepseq, force)
import Prelude hiding (($!))
import Conduit


-- What would happen if, instead of in part1, the code said in part2? How about in answer?
add :: Int -> Int -> Int
add x y =
  let
    part1 = seq x part2
    part2 = seq y answer
    answer = x + y
  in
    part1
    -- part2 -- x isn't evaluated
    -- answer -- x and y aren't evaluated


main1 :: IO ()
main1 = do
  let five = trace "five" $ add (1 + 1) (1 + 2)
      seven = trace "seven" $ add (1 + 2) undefined -- (1 + 3)
  putStrLn $ "Five: " ++ show five
  where
    add :: Int -> Int -> Int
    add !x !y = x + y


main2 :: IO ()
main2 = do
   putStrLn $ ((+) undefined) `seq` "Not throwing. I'm in WHNF."
   putStrLn $ Just undefined `seq` "Not throwing. I'm in WHNF."
   putStrLn $ undefined 5 `seq` "Throwing"
   putStrLn $ (error "foo" :: Int -> Double) `seq` "Throwing"


myDeepSeq :: NFData a => a -> b -> b
myDeepSeq x y =
  rnf x `seq` y


data Foo =
  Foo Int
data Bar =
  Bar !Int
newtype Baz =
  Baz Int


-- it throws since `Baz undefined` ~= `undefined` and `seq` forces
-- its evaluation
main3 :: IO ()
main3 =
  Baz undefined `seq` putStrLn "Still alive!"


mysum :: [Int] -> Int
mysum list0 =
  go list0 0
  where
    go [] total = total
    go (x:xs) total = go xs $! total + x
    -- go (x:xs) total =
    --   let newTotal = total + x in newTotal `seq` go xs newTotal
    -- go (x:xs) total =
    --   let !newTotal = total + x in go xs newTotal


($!) :: (a -> b) -> a -> b
f $! x =
  let y = f x in y `seq` y


($!!) :: NFData b => (a -> b) -> a -> b
f $!! x =
  let y = f x in y `deepseq` y


infixr 0 $!
infixr 0 $!!


{-
Data.Sequence.Seq is defined as follows: `newtype Seq a = Seq (FingerTree (Elem a))`
and FingerTree as follows:
```
data FingerTree a
  = Empty
  | Single a
  | Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
```

To me Seq seems to be just spine strict since the evaluation of the value `a`
is never forced.
-}


average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = (total + x, count + 1)


averageForce :: Monad m => ConduitM Int o m Double
averageForce =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = force (total + x, count + 1)


averageBangs :: Monad m => ConduitM Int o m Double
averageBangs =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x =
      let
        !total' = total + x
        !count' = count + 1
      in
        (total', count')


data StrictPair =
  P !Int !Int


averageCustom :: Monad m => ConduitM Int o m Double
averageCustom =
  divide <$> foldlC add (P 0 0)
  where
    divide :: StrictPair -> Double
    divide (P total count) = fromIntegral total / fromIntegral count
    add (P total count) x = P (total + x) (count + 1)


main :: IO ()
main =
  print $ runConduitPure $ enumFromToC 1 1000000 .| averageCustom


data StrictList a =
  Cons !a !(StrictList a) | Nil


strictMap :: (a -> b) -> StrictList a -> StrictList b
strictMap _ Nil = Nil
strictMap f (Cons a list) =
  let !b = f a
      !list' = strictMap f list
  in b `seq` list' `seq` Cons b list'


strictEnum :: Int -> Int -> StrictList Int
strictEnum low high =
  go low where
  go !x
    | x == high = Cons x Nil
    | otherwise = Cons x (go $! x + 1)


double :: Int -> Int
double !x = x * 2


evens :: StrictList Int
evens = strictMap double $! strictEnum 1 1000000


main5 :: IO ()
main5 = do
  let string = "Hello World"
      -- !string' = evens `seq` string -- max mem usage
      string' = evens `seq` string
  -- putStrLn $ string' `seq` string -- max mem usage
  putStrLn string
