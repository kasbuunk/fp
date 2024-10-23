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

    it "someone has won" $ do
      won empty `shouldBe` False
      won [[X, O, B], [X, O, O], [X, O, X]] `shouldBe` True
      won [[X, O, B], [X, B, O], [X, O, X]] `shouldBe` True

    it "show grid" $ do
      showGrid empty `shouldBe` " | | \n | | \n | | \n"
      showGrid [[X, O, B], [O, X, O], [B, O, X]] `shouldBe` "X|O| \nO|X|O\n |O|X\n"

    it "legal move" $ do
      valid empty 0 `shouldBe` True
      valid empty 8 `shouldBe` True
      valid empty 9 `shouldBe` False
      valid [[X, B, B], [B, B, B], [B, B, B]] 1 `shouldBe` True
      valid [[X, B, B], [B, B, B], [B, B, B]] 0 `shouldBe` False
      valid [[X, B, B], [B, O, B], [B, B, B]] 4 `shouldBe` False
      valid [[X, B, B], [B, B, B], [B, B, B]] 4 `shouldBe` True
      valid [[X, B, B], [B, B, B], [B, B, X]] 8 `shouldBe` False
      valid [[X, B, B], [B, B, B], [B, B, B]] 8 `shouldBe` True
