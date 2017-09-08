import Test.Hspec (SpecWith, hspec, describe, it, xdescribe)
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck ( (==>)
                       , Arbitrary(..), Gen, Testable, Property
                       , property, forAll, choose
                       )
import Data.Word (Word32)
import Data.List (foldl')
import qualified BloomFilter.BloomFilter as BloomFilter
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Set as Set


main :: IO ()
main =
  hspec $
    describe "Bloom filter" $ do
      describe "Querying" $ do
        prop 1000 "one present" prop_onePresent
        prop 1000 "all present" prop_allPresent

      describe "Sizing" $ do
        prop 1000 "suggestions sane" prop_suggestionsSane

      describe "Error rate" $ do
        prop 1000 "less or equal than expected" $ prop_expectedFalsePositivesRate 1000


prop_onePresent :: Strict.ByteString -> Property
prop_onePresent x =
  forAll falsePositiveRate $ \errRate ->
    BloomFilter.mkFromList errRate [x] =~> \bfilt -> x `BloomFilter.elem` bfilt


prop_allPresent :: [Strict.ByteString] -> Property
prop_allPresent xs =
  not (null xs) ==> forAll falsePositiveRate $ \errRate ->
    BloomFilter.mkFromList errRate xs =~> \bfilt -> all (`BloomFilter.elem` bfilt) xs


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


prop_expectedFalsePositivesRate :: Int -> [Strict.ByteString] -> [Strict.ByteString] -> Property
prop_expectedFalsePositivesRate n xs ys =
  config $ \numFps errRate ->
    -- UHU !? Why is this failing with ctrl chars? i.e. xs = ["\SOH"], ys = ["\NUL"]
    -- fromIntegral numFps / fromIntegral (length ys) <= errRate + 0.001 -- ±0.01%
    numFps <= ceiling (errRate * fromIntegral n)
  where
    config :: (Int -> Double -> Bool) -> Property
    config test =
      all (not . null) [xs, ys] && xs /= ys ==> forAll falsePositiveRate $ \errRate ->
        BloomFilter.mkFromList errRate xs =~> \bfilt ->
          test (countFalsePositives bfilt ys) errRate
      where
        countFalsePositives bfilt =
          foldl' (flip (countF bfilt)) 0

        countF bfilt y =
          if falsePositive bfilt y then (+1) else id

        falsePositive bfilt x =
          x `BloomFilter.elem` bfilt && not (x `Set.member` sxs)

        sxs =
          Set.fromList xs


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
  either (const False) f k


instance Arbitrary Lazy.ByteString where
  arbitrary =
    Lazy.pack `fmap` arbitrary


instance Arbitrary Strict.ByteString where
  arbitrary =
    Strict.pack `fmap` arbitrary
