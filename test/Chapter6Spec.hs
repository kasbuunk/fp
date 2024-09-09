module Chapter6Spec where

import Chapter6
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter6" $ do
    it "faculty" $ do
      fac 0 `shouldBe` 1
      fac 1 `shouldBe` 1
      fac 2 `shouldBe` 2
      fac 3 `shouldBe` 6

    it "recursive multiplication" $ do
      recursiveMultiply 0 0 `shouldBe` 0
      recursiveMultiply 1 0 `shouldBe` 0
      recursiveMultiply 0 1 `shouldBe` 0
      recursiveMultiply 1 1 `shouldBe` 1
      recursiveMultiply 6 5 `shouldBe` 30
