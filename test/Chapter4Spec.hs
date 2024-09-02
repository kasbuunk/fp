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
