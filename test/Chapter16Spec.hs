module Chapter16Spec where

import Chapter16
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter16" $ do
    it "power without overlap" $ do
      power 1 0 `shouldBe` 1
      power 2 5 `shouldBe` 32

    it "subtract" $ do
      show (sub (Succ (Succ Zero)) (Succ Zero)) `shouldBe` "Succ Zero"
      show (sub (Succ Zero) (Succ (Succ Zero))) `shouldBe` "Zero"
