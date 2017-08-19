module Ch13.NumSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Ch13.Num


spec :: Spec
spec =
  describe "show" $ do
    it "should show symbolic manip with only plus-es correctly" $ property prop_add


instance Arbitrary Op where
  arbitrary =
    oneof $ fmap pure [Plus, Minus, Mul, Div, Pow]


instance Arbitrary a => Arbitrary (SymbolicManip a) where
  arbitrary =
    oneof [ Number <$> arbitrary
          -- , Symbol <$> arbitrary
          , BinaryArith <$> arbitrary <*> arbitrary <*> arbitrary
          , UnaryArith <$> pure "abs" <*> arbitrary
          ]
    -- sized $ \x -> gensm x (Number <$> arbitrary)
    -- where
      -- gensm n gexpr = do
      --   expr <- gexpr
      --   if size expr == n
      --     then gexpr
      --     else gensm n (BinaryArith <$> arbitrary <*> gexpr <*> (Number <$> arbitrary))

      -- size (BinaryArith _ x y) =
      --   size x + size y
      -- size (UnaryArith _ x) =
      --   size x
      -- size _ =
      --   1


prop_add :: SymbolicManip Int -> Property
prop_add sm@(BinaryArith _ x y) =
  checkPlus sm ==> show sm == show x ++ "+" ++ show y
  where
    checkPlus (BinaryArith Plus x y) =
      checkPlus x && checkPlus y
    checkPlus (BinaryArith _ _ _) =
      False
    checkPlus _ =
      True
prop_add _ =
  property True
