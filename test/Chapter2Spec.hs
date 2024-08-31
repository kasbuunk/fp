module Chapter2Spec where

import Test.Hspec
import Chapter2

spec :: Spec
spec = do
    describe "Chapter2" $ do
        it "import test" $ do
            add_one 0 `shouldBe` 1 
