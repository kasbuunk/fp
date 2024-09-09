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

    it "recursive product" $ do
      product' [] `shouldBe` 1
      product' [5] `shouldBe` 5
      product' [1, 2, 3] `shouldBe` 6

    it "recursive length" $ do
      length' [] `shouldBe` 0
      length' [1] `shouldBe` 1
      length' [1, 2] `shouldBe` 2

    it "recursive reverse" $ do
      reverse' [] `shouldBe` ([] :: [Int])
      reverse' ['a'] `shouldBe` ['a']
      reverse' [1, 2] `shouldBe` [2, 1]
      reverse' [1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]

    it "recursive append" $ do
      append [] [0] `shouldBe` ([0])
      append [0] [] `shouldBe` ([0])
      append [1] [2] `shouldBe` ([1, 2])
      append [1, 3] [2, 4] `shouldBe` ([1, 3, 2, 4])

    it "recursive insert" $ do
      insert 3 [1, 2, 4, 5] `shouldBe` [1, 2, 3, 4, 5]

    it "insertion sort" $ do
      insertionsort [4, 2, 5, 1, 3] `shouldBe` [1, 2, 3, 4, 5]

    it "reinvent zip" $ do
      zip' [1, 2, 3] ['a', 'b', 'c'] `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
      zip' [1, 2, 3] ['a', 'b'] `shouldBe` [(1, 'a'), (2, 'b')]

    it "reinvent drop" $ do
      drop' 0 [1] `shouldBe` [1]
      drop' 0 [1, 2, 3] `shouldBe` [1, 2, 3]
      drop' 1 [1, 2, 3] `shouldBe` [2, 3]
      drop' 2 [1, 2, 3] `shouldBe` [3]
      drop' 3 [1, 2, 3] `shouldBe` []
