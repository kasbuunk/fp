module Chapter8Spec where

import Chapter8
import Test.Hspec

floatEq :: Float -> Float -> Float -> Bool
floatEq margin x y = (x - y) / y <= margin

spec :: Spec
spec = do
  describe "Chapter8" $ do
    it "dictionary" $ do
      let t = [("foo", 0), ("bar", 1), ("key", 7), ("baz", 10)]
       in find "key" t `shouldBe` 7

    it "move positions" $ do
      moves [North, North] (0, 0) `shouldBe` (0, 2)
      moves [West, South] (0, 0) `shouldBe` (-1, -1)
      moves [North, North, West, South, East, West, North] (0, 0) `shouldBe` (-1, 2)

    it "shape areas" $ do
      floatEq 0.001 (area (Circle 1)) pi `shouldBe` True
      floatEq 0.001 (area (Rectangle 4 6)) 24 `shouldBe` True

    it "safediv" $ do
      safediv 20 0 `shouldBe` Nothing
      safediv 20 2 `shouldBe` Just 10

    it "safehead" $ do
      safehead ([] :: [Int]) `shouldBe` Nothing
      safehead [20, 2] `shouldBe` Just 20
