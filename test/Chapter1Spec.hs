module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1 (add_one)

spec :: Spec
spec = do
    describe "Chapter 1 Tests" $ do
        it "add one" $ do
            add_one 0 `shouldBe` 1
