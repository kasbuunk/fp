module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1

spec :: Spec
spec = do
    describe "Chapter1" $ do
        it "import test" $ do
            add_one 0 `shouldBe` 1

        -- W1.1 means exercise 1.1 in the work book.
        it "W1.1: surface square" $ do
            square 5 `shouldBe` 25

        it "T1.3a: generate list of numbers" $ do
            numbers 6 `shouldBe` [1,2,3,4,5,6]

        it "T1.3b: numbers 5" $ do
            numbers 5 `shouldBe` [1,2,3,4,5]

        it "T1.3c: sum from 1 to 5" $ do
            sum (numbers 5) `shouldBe` 15
