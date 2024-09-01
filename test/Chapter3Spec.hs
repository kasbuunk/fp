module Chapter3Spec where

import Test.Hspec
import Chapter3

spec :: Spec
spec = do
    describe "Chapter 3" $ do
        it "add numbers in a tuple" $ do
            add (4, 6) `shouldBe` 10

        it "generate list from zero to n" $ do
            zeroto 8 `shouldBe` [0,1,2,3,4,5,6,7,8]

        it "try Eq on Bool" $ do
            False == False `shouldBe` True

        it "try Eq on Char" $ do
            'a' == 'b' `shouldBe` False

        it "try Eq on String" $ do
            "abc" == "abc" `shouldBe` True

        it "try Eq on tuple" $ do
            not (('a', False) == ('a', False)) `shouldBe` False

        it "try Eq on list" $ do
            [1,2] == [1,2,3] `shouldBe` False
