module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1 (add_one, double, quadruple)

spec :: Spec
spec = do
    describe "Chapter 1 Tests" $ do
        it "add one" $ do
            add_one 0 `shouldBe` 1

        it "double 2" $ do
            double 2 `shouldBe` 4

        it "double 4" $ do
            double 4 `shouldBe` 8

        it "quadruple 3" $ do
            quadruple 3 `shouldBe` 12

        it "quadruple 5" $ do
            quadruple 5 `shouldBe` 20
