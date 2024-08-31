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

        -- T1.1 means exercise 1 in chapter 1 in the text book.
        it "T1.1: calculate double (double 2)" $ do
            double_twice 2 `shouldBe` double (double 2)
