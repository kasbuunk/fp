module Chapter15Spec where

import Chapter15
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter15" $ do
    it "multiplyByPosition" $ do
      multByPos [1, 1, 1, 1, 1, 1] `shouldBe` [1, 2, 3, 4, 5, 6]
      multByPos [1, 1, 2, 2, 3, 3] `shouldBe` [1, 2, 6, 8, 15, 18]
