module Chapter8Spec where

import Chapter8
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter8" $ do
    it "dictionary" $ do
      let t = [("foo", 0), ("bar", 1), ("key", 7), ("baz", 10)]
       in find "key" t `shouldBe` 7
