module Chapter11Spec where

import Chapter11
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter11" $ do
    it "next player" $ do
      next O `shouldBe` X
      next X `shouldBe` O
      next B `shouldBe` B

    it "empty grid" $ do
      empty `shouldBe` [[B, B, B], [B, B, B], [B, B, B]]

    it "empty grid is not full" $ do
      full empty `shouldBe` False

    it "full grid is full" $ do
      full (replicate size (replicate size O)) `shouldBe` True
