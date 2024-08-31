module Chapter3Spec where

import Test.Hspec
import Chapter3

spec :: Spec
spec = do
    describe "Chapter 3" $ do
        it "add numbers in a tuple" $ do
            add (4, 6) `shouldBe` 10
