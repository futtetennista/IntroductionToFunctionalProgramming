import Test.Hspec (SpecWith, hspec, describe, it, xdescribe)
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck ( (==>)
                       , Arbitrary(..), Gen, Testable, Property
                       , property, forAll, choose
                       )
import Data.Word (Word32)
import qualified BloomFilter.BloomFilter as BloomFilter
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.List (foldl')


main :: IO ()
main =
  hspec $
    describe "Bloom filter" $ do
      xdescribe "Querying" $ do
        prop 1000 "one present" prop_onePresent
        prop 1000 "all present" prop_allPresent

      xdescribe "Sizing" $ do
        prop 1000 "suggestions sane" prop_suggestionsSane

      describe "Error rate expectations" $ do
        prop 100 "false positives rate" prop_expectedFalsePositivesRate


prop_onePresent :: Strict.ByteString -> Property
prop_onePresent elt =
  forAll falsePositiveRate $ \errRate ->
    BloomFilter.easyList errRate [elt] =~> \filt -> elt `BloomFilter.elem` filt


prop_allPresent :: [Strict.ByteString] -> Property
prop_allPresent xs =
  forAll falsePositiveRate $ \errRate ->
    BloomFilter.easyList errRate xs =~> \filt -> all (`BloomFilter.elem` filt) xs


prop_suggestionsSane :: Property
prop_suggestionsSane =
  config $ \capacity errRate ->
    either (const False) sane $ BloomFilter.suggestSizing capacity errRate
  where
    config test =
      forAll falsePositiveRate $ \errRate ->
        forAll (choose (1, fromIntegral maxWord32 `div` 8)) $ \capacity ->
          let
            size =
              fst . minimum $ BloomFilter.sizings capacity errRate
          in
            size < fromIntegral maxWord32 ==> test capacity errRate

    sane (bits, hashes) =
      bits > 0 && bits < maxBound && hashes > 0

    maxWord32 =
      maxBound :: Word32


prop_expectedFalsePositivesRate :: [Strict.ByteString] -> [Strict.ByteString] -> Property
prop_expectedFalsePositivesRate xs ys =
  config $ \numFps errRate ->
    errRate - tolerance >= fromIntegral numFps / fromIntegral (length ys)
      && fromIntegral numFps / fromIntegral (length ys) <= errRate + tolerance
  where
    -- ±1%
    tolerance =
      0.01

    config :: (Int -> Double -> Bool) -> Property
    config test =
      forAll falsePositiveRate $ \errRate ->
        not (null xs)  && not (null ys) ==>
          BloomFilter.easyList errRate xs =~> \bfilt ->
            let
              numFalsePositives =
                foldl' (flip (countFps bfilt)) 0 ys
            in
              test numFalsePositives errRate
      where
        countFps bfilt y =
          if y `BloomFilter.elem` bfilt then (+1) else id


prop :: Testable prop => Int -> String -> prop -> SpecWith ()
prop size title =
  modifyMaxSize (const size) . it title . property


falsePositiveRate :: Gen Double
falsePositiveRate =
  choose (epsilon, 1 - epsilon)
  where
    epsilon =
      1e-6


(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f =
  either (const True) f k


instance Arbitrary Lazy.ByteString where
  arbitrary =
    Lazy.pack `fmap` arbitrary


instance Arbitrary Strict.ByteString where
  arbitrary =
    Strict.pack `fmap` arbitrary
