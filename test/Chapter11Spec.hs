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

    it "whose turn" $ do
      turn [[B, B, B], [B, B, B], [B, B, B]] `shouldBe` O
      turn [[O, B, B], [B, B, B], [B, B, B]] `shouldBe` X
      turn [[X, O, B], [B, B, B], [B, B, B]] `shouldBe` O

    it "player wins" $ do
      wins X [[X, O, B], [B, B, B], [X, X, X]] `shouldBe` True
      wins O [[X, O, B], [B, B, B], [X, X, X]] `shouldBe` False
      wins O [[X, O, O], [B, O, B], [O, X, X]] `shouldBe` True
      wins X [[X, O, O], [B, O, B], [O, X, X]] `shouldBe` False
      wins X [[X, O, B], [X, B, O], [X, O, X]] `shouldBe` True
      wins O [[X, O, B], [X, B, O], [X, O, X]] `shouldBe` False
      wins O [[X, O, B], [X, O, O], [X, O, X]] `shouldBe` True
