module Chapter4Spec (spec)
where

import Test.Hspec
import Chapter4


spec :: Spec
spec =
  do
    describe "4.1: Convert numbers to words" $ do
      it "should convert numbers to words" $ do
        convert 23 `shouldBe` "twenty-three"
        convert 123 `shouldBe` "one hundred and twenty-three"
        convert 308000 `shouldBe` "three hundred and eight thousand"
        convert 369027 `shouldBe` "three hundred and sixty-nine thousand and twenty-seven"
        convert 369401 `shouldBe` "three hundred and sixty-nine thousand four hundred and one"

      it "should convert numbers to words and adds full stop at the end" $ do
        convertWithFullStop 23 `shouldBe` "twenty-three."
        convertWithFullStop 123 `shouldBe` "one hundred and twenty-three."
        convertWithFullStop 308000 `shouldBe` "three hundred and eight thousand."
        convertWithFullStop 369027 `shouldBe` "three hundred and sixty-nine thousand and twenty-seven."
        convertWithFullStop 369401 `shouldBe` "three hundred and sixty-nine thousand four hundred and one."

      it "should convert numbers to words up to one billion exclusive" $ do
        pending
        -- convertWithFullStop 3080000 `shouldBe` "three million and eighty thousand."
        -- convertWithFullStop 3690270 `shouldBe` "three million six hundred and ninety hundred thousand and two hundred and seventy."
        -- convertWithFullStop 369270000 `shouldBe` "three hundred and sixty-nine million and two hundred and sixty-nine thousand."

      it "should convert negative numbers to words" $ do
        convertNatural (-23) `shouldBe` "minus twenty-three."
        convertNatural (-123) `shouldBe` "minus one hundred and twenty-three."
        convertNatural (-308000) `shouldBe` "minus three hundred and eight thousand."

      it "should convert a whole number of pence into words" $ do
        convertMoney 3649 `shouldBe` "thirty-six pounds and forty-nine pence"
        convertMoney 3600049 `shouldBe` "thirty-six thousand pounds and forty-nine pence"

      it "should convert words to numbers" $ do
        numToNum 6 `shouldBe` 6
        numToNum 13 `shouldBe` 13
        numToNum 23 `shouldBe` 23
        numToNum 123 `shouldBe` 123
        numToNum 1023 `shouldBe` 1023
        numToNum 1123 `shouldBe` 1123
        numToNum 308000 `shouldBe` 308000
        numToNum 369027 `shouldBe` 369027
        numToNum 369401 `shouldBe` 369401
          where
            numToNum x =
              reverseConvert . convert $ x
