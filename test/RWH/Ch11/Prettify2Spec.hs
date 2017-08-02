module RWH.Ch11.Prettify2Spec (spec)
where

import Test.Hspec
import Test.QuickCheck
import RWH.Ch11.Prettify2


spec :: Spec
spec =
  describe "empty" $ do
    it "should not change the doc when ap- or pre-prending to empty doc" $
      property (prop_empty_id)


instance Arbitrary Doc where
  arbitrary =
    oneof [ pure Empty
          , Char <$> arbitrary
          , Text <$> arbitrary
          , pure Line
          , Concat <$> arbitrary <*> arbitrary
          , Union <$> arbitrary <*> arbitrary
          ]
    -- n <- choose (1,6) :: Gen Int
    -- case n of
    --   1 ->
    --     return Empty

    --   2 ->
    --     Char <$> arbitrary

    --   3 ->
    --     Text <$> arbitrary

    --   4 ->
    --     return Line

    --   5 ->
    --     Concat <$> arbitrary <*> arbitrary

    --   6 ->
    --     Union ($) arbitrary <*> arbitrary


prop_empty_id x =
  empty <> x == x && x <> empty == x


prop_char c =
  char c == Char c


prop_text s =
  text s == if null s then Empty else Text s


prop_line =
  line == Line


-- prop_double d = double d == text (show d)
