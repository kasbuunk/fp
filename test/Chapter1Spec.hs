module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1

spec :: Spec
spec = do
    describe "Chapter1" $ do
        it "import test" $ do
            add_one 0 `shouldBe` 1

        it "Exercise 1.1: surface square" $ do
            square 5 `shouldBe` 25
