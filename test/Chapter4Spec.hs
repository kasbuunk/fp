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

        it "split list at n" $ do
            splitAt' [0, 1, 2, 3, 4] 3 `shouldBe` ([0, 1, 2], [3, 4])

        it "reciprocal" $ do
            reciprocal 10 `shouldBe` 0.1
            reciprocal 0.5 `shouldBe` 2

        it "abs with if-then-else" $ do
            abs' 0 `shouldBe` 0
            abs' 1 `shouldBe` 1
            abs' (-1) `shouldBe` 1
            abs' (-10.1) `shouldBe` 10.1

        it "signum with nested if-then-else" $ do
            signum' 0 `shouldBe` 0
            signum' 1.9 `shouldBe` 1
            signum' 2 `shouldBe` 1
            signum' (-1) `shouldBe` -1
            signum' (-2.5) `shouldBe` -1

        it "abs with guards" $ do
            abs'' 0 `shouldBe` 0
            abs'' 1 `shouldBe` 1
            abs'' (-1) `shouldBe` 1
            abs'' (-10.1) `shouldBe` 10.1

        it "signum with guards" $ do
            signum'' 0 `shouldBe` 0
            signum'' 1.9 `shouldBe` 1
            signum'' 2 `shouldBe` 1
            signum'' (-1) `shouldBe` -1
            signum'' (-2.5) `shouldBe` -1

        it "not with pattern matching" $ do
            not' True `shouldBe` False
            not' False `shouldBe` True

        it "and with pattern matching" $ do
            and' True True `shouldBe` True
            and' True False `shouldBe` False
            and' False True `shouldBe` False
            and' False False `shouldBe` False
            and'' True True `shouldBe` True
            and'' True False `shouldBe` False
            and'' False True `shouldBe` False
            and'' False False `shouldBe` False
            and''' True True `shouldBe` True
            and''' True False `shouldBe` False
            and''' False True `shouldBe` False
            and''' False False `shouldBe` False
            and'''' True True `shouldBe` True
            and'''' True False `shouldBe` False
            and'''' False True `shouldBe` False
            and'''' False False `shouldBe` False

        it "Tuple with pattern match" $ do
            let testCase = (5, "second")
            first testCase `shouldBe` 5
            second testCase `shouldBe` "second"

        it "list with pattern match" $ do
            let startsWith_a = startsWith 'a'
            startsWith_a ['a', 'b', 'c'] `shouldBe` True
            startsWith_a ['b', 'c'] `shouldBe` False
            startsWith 1 [1,2,3] `shouldBe` True
            startsWith (0,"123") [(0,"123")] `shouldBe` True
            startsWith [] [[0],[]] `shouldBe` False

        it "head with pattern match" $ do
            head' ['a'] `shouldBe` 'a'
            head' ['a', 'b'] `shouldBe` 'a'

        it "tail with pattern match" $ do
            tail' ['a'] `shouldBe` []
            tail' ['a', 'b'] `shouldBe` ['b']
            tail' ['a', 'b', 'c'] `shouldBe` ['b', 'c']
            tail' [1,2,3] `shouldBe` [2,3]

        it "add with lambda" $ do
            add' 0 1 `shouldBe` 1

        it "const with lambda" $ do
            const 1 'a' `shouldBe` 1
            const' 1 'a' `shouldBe` 1

        it "n odd integers" $ do
            odds' 3 `shouldBe` [1,3,5]
            odds'' 3 `shouldBe` [1,3,5]

        it "operator sections" $ do
            (+) 3 2 `shouldBe` 5
            (+3) 2 `shouldBe` 5
            (3+) 2 `shouldBe` 5
            (1/) 8 `shouldBe` 0.125
            (/10) 100 `shouldBe` 10

        it "sum as fold" $ do
            sum' [0,1,5,1] `shouldBe` 7

        it "halve a list" $ do
            halve [1,2] `shouldBe` ([1],[2])
            halve [1,2,3,4,5,6] `shouldBe` ([1,2,3],[4,5,6])

        it "third element" $ do
            third [1,2,3] `shouldBe` 3
            third [0,1,2,3,4,5] `shouldBe` 2
            third' [1,2,3] `shouldBe` 3
            third' [0,1,2,3,4,5] `shouldBe` 2
            third'' [1,2,3] `shouldBe` 3
            third'' [0,1,2,3,4,5] `shouldBe` 2

        it "safetail" $ do
            safetail [] `shouldBe` ([] :: [Int]) -- type annotation needed for inference.
            safetail [1] `shouldBe` []
            safetail [1,2] `shouldBe` [2]
            safetail ['a','b','c'] `shouldBe` ['b','c']
            safetail' [] `shouldBe` ([] :: [Int]) -- type annotation needed for inference.
            safetail' [1] `shouldBe` []
            safetail' [1,2] `shouldBe` [2]
            safetail' ['a','b','c'] `shouldBe` ['b','c']
            safetail'' [] `shouldBe` ([] :: [Int]) -- type annotation needed for inference.
            safetail'' [1] `shouldBe` []
            safetail'' [1,2] `shouldBe` [2]
            safetail'' ['a','b','c'] `shouldBe` ['b','c']

        it "disjunction with pattern matching" $ do
            or' True True `shouldBe` True
            or' True False `shouldBe` True
            or' False True `shouldBe` True
            or' False False `shouldBe` False
            or'' True True `shouldBe` True
            or'' True False `shouldBe` True
            or'' False True `shouldBe` True
            or'' False False `shouldBe` False
            or''' True True `shouldBe` True
            or''' True False `shouldBe` True
            or''' False True `shouldBe` True
            or''' False False `shouldBe` False
            or'''' True True `shouldBe` True
            or'''' True False `shouldBe` True
            or'''' False True `shouldBe` True
            or'''' False False `shouldBe` False

        it "luhn double" $ do
            luhnDouble 3 `shouldBe` 6
            luhnDouble 6 `shouldBe` 3

        it "luhn algorithm" $ do
            luhn 1 7 8 4 `shouldBe` True
            luhn 4 7 8 3 `shouldBe` False

        it "surface rectangle" $ do
            surfaceRectangle 10 5 `shouldBe` 50
            surfaceRectangle 10.5 2.0 `shouldBe` 21.0

        it "volume cuboid" $ do
            volumeCuboid 10 5 2 `shouldBe` 100
            volumeCuboid 10.5 2.0 3.0 `shouldBe` 63.0

        it "minimum" $ do
            min2 0 1 `shouldBe` 0
            min2 10.1 1.5 `shouldBe` 1.5
            min2' 0 1 `shouldBe` 0
            min2' 10.1 1.5 `shouldBe` 1.5
            min3 0 1 4 `shouldBe` 0
            min3 10.1 1.5 4.3 `shouldBe` 1.5
            min3' 0 1 10 `shouldBe` 0
            min3' 10.1 1.5 4.3`shouldBe` 1.5

        it "abc formula" $ do
            abc (1, 0, 1) `shouldBe` []
            abc (1, 0, 0) `shouldBe` [0]
            abc (1, -1, 0) `shouldBe` [1, 0]
            abc' 1 0 1 `shouldBe` []
            abc' 1 0 0 `shouldBe` [0]
            abc' 1 (-1) 0 `shouldBe` [1, 0]

        it "number of roots of quadratic equation" $ do
            rootsQuadratic 1 0 1 `shouldBe` 0
            rootsQuadratic 1 0 0 `shouldBe` 1
            rootsQuadratic 1 (-1) 0 `shouldBe` 2
            rootsQuadratic' 1 0 1 `shouldBe` 0
            rootsQuadratic' 1 0 0 `shouldBe` 1
            rootsQuadratic' 1 (-1) 0 `shouldBe` 2

        it "is positive" $ do
            isPositive 1 `shouldBe` True
            isPositive 0 `shouldBe` False
            isPositive (-1) `shouldBe` False
