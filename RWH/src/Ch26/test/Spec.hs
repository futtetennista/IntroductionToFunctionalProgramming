import Test.Hspec (SpecWith, hspec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck ( (==>)
                       , Arbitrary(..), Gen, Testable, Property
                       , property, forAll, choose
                       )
import Data.Word (Word32)
import qualified BloomFilter.Utils as Utils
import qualified BloomFilter.Immutable as IBloom
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy


main :: IO ()
main =
  hspec $
    describe "Bloom filter" $ do
      prop "one present" prop_onePresent
      prop "all present" prop_allPresent
      prop "suggestions sane" prop_suggestionsSane


prop_onePresent :: Strict.ByteString -> Property
prop_onePresent elt =
  forAll falsePositive $ \errRate ->
    Utils.easyList errRate [elt] =~> \filt -> elt `IBloom.elem` filt


prop_allPresent :: [Strict.ByteString] -> Property
prop_allPresent xs =
  forAll falsePositive $ \errRate ->
    Utils.easyList errRate xs =~> \filt -> all (`IBloom.elem` filt) xs


prop_suggestionsSane :: Property
prop_suggestionsSane =
  config $ \capacity errRate ->
    either (const False) sane $ Utils.suggestSizing capacity errRate
  where
    config test =
      forAll falsePositive $ \errRate ->
        forAll (choose (1, fromIntegral maxWord32 `div` 8)) $ \capacity ->
          let
            size =
              fst . minimum $ Utils.sizings capacity errRate
          in
            size < fromIntegral maxWord32 ==> test capacity errRate

    sane (bits, hashes) =
      bits > 0 && bits < maxBound && hashes > 0

    maxWord32 =
      maxBound :: Word32


prop :: Testable prop => String -> prop -> SpecWith ()
prop title =
  modifyMaxSize (const 1000) . it title . property


falsePositive :: Gen Double
falsePositive =
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
