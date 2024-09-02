module Chapter4Spec where

import Test.Hspec
import Chapter4

spec :: Spec
spec = do
    describe "Chapter 4" $ do
        it "is even" $ do
            even' 0 `shouldBe` True
            even' 1 `shouldBe` False
            even' 2 `shouldBe` True
            even' (-1) `shouldBe` False
            even' (-2) `shouldBe` True

        it "split list at n" $ do
            splitAt' [0, 1, 2, 3, 4] 3 `shouldBe` ([0, 1, 2], [3, 4])

        it "reciprocal" $ do
            reciprocal 10 `shouldBe` 0.1
            reciprocal 0.5 `shouldBe` 2

        it "abs" $ do
            abs' 0 `shouldBe` 0
            abs' 1 `shouldBe` 1
            abs' (-1) `shouldBe` 1
            abs' (-10.1) `shouldBe` 10.1

        it "signum" $ do
            signum' 0 `shouldBe` 0
            signum' 1.9 `shouldBe` 1
            signum' 2 `shouldBe` 1
            signum' (-1) `shouldBe` -1
            signum' (-2.5) `shouldBe` -1
