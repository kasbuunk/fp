module Chapter5Spec where

import Test.Hspec
import Chapter5

spec :: Spec
spec = do
    describe "Chapter 5" $ do
        it "squares of integers 1 to 5" $ do
            [x^2 | x <- [1..5]] `shouldBe` [1,4,9,16,25]

        it "cartesian product" $ do
            cartesian [1,2,3] [4,5] `shouldBe` [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

        it "second generator depends on first" $ do
            [(x,y) | x <- [1..3], y <- [x..3]] `shouldBe` [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

        it "concatenate list of lists" $ do
            concatenate [[0,1],[2,3],[4]] `shouldBe` [0,1,2,3,4]
