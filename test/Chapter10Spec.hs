module Chapter10Spec where

import Chapter10
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter10" $ do
    it "isNumber" $ do
      isNumber' "10" `shouldBe` True
