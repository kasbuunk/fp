module Chapter8Spec where

import Chapter8
import Test.Hspec

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
