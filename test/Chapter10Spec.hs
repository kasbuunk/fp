module Chapter10Spec where

import Chapter10
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter10" $ do
    it "isNumber" $ do
      isNumber' "a" `shouldBe` False
      isNumber' "9" `shouldBe` True
      isNumber' "10" `shouldBe` True
      isNumber' "a0" `shouldBe` False

    it "nextgen game of life" $ do
      nextgen glider `shouldBe` [(4, 3), (3, 4), (4, 4), (3,2), (5,3)]
