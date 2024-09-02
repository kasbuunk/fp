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
