module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1

spec :: Spec
spec = do
    describe "Chapter1" $ do
        it "import test" $ do
            add_one 0 `shouldBe` 1
