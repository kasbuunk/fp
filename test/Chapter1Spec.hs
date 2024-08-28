module Chapter1Spec (spec) where

import Test.Hspec
import Chapter1 (add_one, double, quadruple)

spec :: Spec
spec = do
    describe "Chapter 1 Tests" $ do
        it "add one" $ do
            add_one 0 `shouldBe` 1

        it "double integers" $ do
            mapM_ (\(input, expected) -> double input `shouldBe` expected)
                [ (0, 0)
                , (2, 4)
                , (-2, -4)
                , (4, 8)
                , (5, 10)
                , (10, 20)
                ]


        it "quadruple 3" $ do
            quadruple 3 `shouldBe` 12

        it "quadruple 5" $ do
            quadruple 5 `shouldBe` 20
