module Chapter15Spec where

import Chapter15
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter15" $ do
    it "thing" $ do
      True `shouldBe` True
