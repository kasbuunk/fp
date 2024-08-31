module Chapter2Spec where

import Test.Hspec
import Chapter2

spec :: Spec
spec = do
    describe "Chapter2" $ do
        it "import test" $ do
            add_one 0 `shouldBe` 1 

        it "get head from list" $ do
            head [1,2,3,4,5] `shouldBe` 1

        it "get tail from list" $ do
            tail [1,2,3,4,5] `shouldBe` [2,3,4,5]

        it "select nth element from list" $ do
            [1,2,3,4,5]!!3 `shouldBe` 4
