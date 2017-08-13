module RWH.Ch11.Prettify2Spec (spec)
where

import Test.Hspec
import Test.QuickCheck
import RWH.Ch11.Prettify2


spec :: Spec
spec =
  describe "doc" $ do
    it "should obey the first monoid law" $ property (prop_mempty_id)
    it "should obey the second monoid law" $ property (prop_mappend_assoc)


instance Arbitrary Doc where
  arbitrary =
    oneof [ pure Empty
          , Char <$> arbitrary
          , Text <$> arbitrary
          , pure Line
          , Concat <$> arbitrary <*> arbitrary
          , Union <$> arbitrary <*> arbitrary
          ]
    -- do
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

prop_mempty_id :: Doc -> Bool
prop_mempty_id x =
  mempty `mappend` x == x && x `mappend` mempty == x


prop_mappend_assoc :: Doc -> Doc -> Doc -> Bool
prop_mappend_assoc x y z =
  (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)


prop_char c =
  char c == Char c


prop_text s =
  text s == if null s then Empty else Text s


prop_line =
  line == Line


-- prop_double d = double d == text (show d)
