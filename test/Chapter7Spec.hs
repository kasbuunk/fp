module Chapter7Spec where

import Chapter7
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter7" $ do
    it "apply twice" $ do
      let add1 = add' 1
      twice add1 0 `shouldBe` 2
      twice reverse [1, 2, 3] `shouldBe` [1, 2, 3]
      twice (2 *) 5 `shouldBe` 20
      twice (* 2) 5 `shouldBe` 20

    it "redefine map" $ do
      map' (+ 1) [0, 1, 2] `shouldBe` [1, 2, 3]
      map' even [0, 1, 2] `shouldBe` [True, False, True]
      map' reverse ["abc", "def", "ghi"] `shouldBe` ["cba", "fed", "ihg"]
      map' (map' (+ 1)) [[0, 1], [2, 3]] `shouldBe` [[1, 2], [3, 4]]

    it "redefine map recursively" $ do
      map'' (+ 1) [0, 1, 2] `shouldBe` [1, 2, 3]
      map'' even [0, 1, 2] `shouldBe` [True, False, True]
      map'' reverse ["abc", "def", "ghi"] `shouldBe` ["cba", "fed", "ihg"]
      map'' (map' (+ 1)) [[0, 1], [2, 3]] `shouldBe` [[1, 2], [3, 4]]

    it "redefine filter" $ do
      filter' (\_ -> False) [0, 1, 2, 3] `shouldBe` []
      filter' (\_ -> True) [0, 1, 2, 3] `shouldBe` [0, 1, 2, 3]
      filter' (\x -> x > 1) [0, 1, 2, 3] `shouldBe` [2, 3]
      filter' (\x -> x == 1) [0, 1, 2, 3] `shouldBe` [1]
      filter' even [0 .. 10] `shouldBe` [0, 2, 4, 6, 8, 10]
      filter' (> 2) [0 .. 5] `shouldBe` [3, 4, 5]
      filter' (/= ' ') "this sentence without spaces" `shouldBe` "thissentencewithoutspaces"

    it "redefine filter recursively" $ do
      filter'' even [0 .. 10] `shouldBe` [0, 2, 4, 6, 8, 10]

    it "sum of squares is even" $ do
      sumsqreven [0, 1, 3] `shouldBe` 0
      sumsqreven [0 .. 6] `shouldBe` 56

    it "all with predicate" $ do
      all even [0, 2, 6, 4, 8] `shouldBe` True

    it "any with predicate" $ do
      any odd [0, 1, 2] `shouldBe` True
      any odd [0, 10, 2] `shouldBe` False
