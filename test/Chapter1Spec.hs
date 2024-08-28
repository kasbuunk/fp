module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1 (add_one)

spec :: Spec
spec = do
    describe "Chapter 1 Tests" $ do
        it "example test 1" $ do
            1 `shouldBe` 1

        it "example test 2" $ do
            2 `shouldBe` 2

        it "add one" $ do
            add_one 0 `shouldBe` 1
