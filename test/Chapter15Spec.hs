module Chapter15Spec where

import Chapter15
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter15" $ do
    it "multiplyByPosition" $ do
      multByPos [1, 1, 1, 1, 1, 1] `shouldBe` [1, 2, 3, 4, 5, 6]
      multByPos [1, 1, 2, 2, 3, 3] `shouldBe` [1, 2, 6, 8, 15, 18]
      multByPos' [1, 1, 1, 1, 1, 1] `shouldBe` [1, 2, 3, 4, 5, 6]
      multByPos' [1, 1, 2, 2, 3, 3] `shouldBe` [1, 2, 6, 8, 15, 18]

    it "power lists" $ do
      take 5 (powers 2) `shouldBe` [1, 2, 4, 8, 16]
      take 5 (powers 3) `shouldBe` [1, 3, 9, 27, 81]

    it "first powers" $ do
      firstPowers 10 2 `shouldBe` [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]

    it "sum powers of n" $ do
      sumPowers 4 2 `shouldBe` 15
      sumPowers 5 2 `shouldBe` 31

    it "next sqrt approximation" $ do
      next 3.0 1.0 `shouldBe` 2.0
      next 3.0 2.0 `shouldBe` 1.75
