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

        it "False is lower than True" $ do
            False < True `shouldBe` True

        it "a is the lowest of a and b" $ do
            min 'a' 'b' `shouldBe` 'a'

        it "elegant < elephant" $ do
            "elegant" < "elephant" `shouldBe` True

        it "list is ordered lexicographically" $ do
            -- Like a phone book, 1 and 2 are equal
            -- and a missing third element is 'lower'
            -- than the 3.
            [1,2] < [1,2,3] `shouldBe` True

        it "tuple is ordered lexicographically" $ do
            -- The 'a' is compared first.
            ('a', 2) < ('b', 1) `shouldBe` True

        it "second value of tuple is considered when the first are equal" $ do
            ('a', 2) > ('a', 1) `shouldBe` True

        it "show False" $ do
            show False `shouldBe` "False"

        it "show Char" $ do
            show 'a' `shouldBe` "'a'"

        it "show Int" $ do
            show 123 `shouldBe` "123"

        it "show Tuple" $ do
            show (1, 'a', False) `shouldBe` "(1,'a',False)"

        it "read Bool" $ do
            (read "False" :: Bool) `shouldBe` False

        it "read Char" $ do
            (read "'a'" :: Char) `shouldBe` 'a'

        it "read String" $ do
            (read "\"abc\"" :: String) `shouldBe` "abc"

        it "read list" $ do
            (read "[1,2,3]" :: [Int]) `shouldBe` [1,2,3]

        it "read Tuple" $ do
            (read "(1, False, ['a','b'])" :: (Int, Bool, [Char])) `shouldBe` (1, False, ['a','b'])

        it "(+) Num: infix" $ do
            1 + 2 `shouldBe` 3

        it "(+) Num: prefix" $ do
            (+) 1 2 `shouldBe` 3

        it "(+) Num: Float" $ do
            1.0 + 2.0 `shouldBe` 3.0

        it "negate Num: Int" $ do
            negate 1 `shouldBe` -1

        it "negate Num: Float" $ do
            negate 1.0 `shouldBe` -1.0

        it "multiply Num" $ do
            5 * 9.0 `shouldBe` 45.0

        it "signum Num" $ do
            signum (-12) `shouldBe` -1

        it "absolute value" $ do
            abs (-12) `shouldBe` abs 12
