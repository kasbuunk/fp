module Chapter2Spec where

import Chapter2
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter2" $ do
    it "import test" $ do
      add_one 0 `shouldBe` 1

    it "get head from list" $ do
      head [1, 2, 3, 4, 5] `shouldBe` 1

    it "get tail from list" $ do
      tail [1, 2, 3, 4, 5] `shouldBe` [2, 3, 4, 5]

    it "select nth element from list" $ do
      [1, 2, 3, 4, 5] !! 3 `shouldBe` 4

    it "take the first n elements of a list" $ do
      take 3 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3]

    it "remove the first n elements from a list" $ do
      drop 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5]

    it "calculate the length of a list" $ do
      length [1, 2, 3, 4, 5] `shouldBe` 5

    it "calculate the sum of a list of numbers" $ do
      sum [1, 2, 3, 4, 5] `shouldBe` 15

    it "calculate the product of a list of numbers" $ do
      product [1, 2, 3, 4, 5] `shouldBe` 120

    it "append two lists" $ do
      [1, 2, 3] ++ [4, 5] `shouldBe` [1, 2, 3, 4, 5]

    it "reverse a list" $ do
      reverse [1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]

    it "quadruple number" $ do
      quadruple 5 `shouldBe` 20

    it "factorial" $ do
      factorial 5 `shouldBe` 1 * 2 * 3 * 4 * 5

    it "average" $ do
      average [1, 2, 9, 10, 3] `shouldBe` 5

    it "alternative init'" $ do
      init' [1, 2, 3, 4, 5] `shouldBe` init [1, 2, 3, 4, 5]

    it "sort characters in string" $ do
      sort_string "functioneel" `shouldBe` "ceefilnnotu"
