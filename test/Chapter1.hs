module Chapter1 (spec) where

import Test.Hspec

spec :: Spec
spec = do
    describe "Chapter 1 Tests" $ do
        it "example test 1" $ do
            1 `shouldBe` 1

        it "example test 2" $ do
            2 `shouldBe` 2
