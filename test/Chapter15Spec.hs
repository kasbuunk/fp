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
