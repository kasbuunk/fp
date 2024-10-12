module Chapter9Spec where

import Chapter9
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter9" $ do
    it "show expression" $ do
      show (App Add (Val 1) (App Mul (Val 2) (Val 3))) `shouldBe` "(1+(2*3))"

    it "subsequences" $ do
      subs [1, 2, 3] `shouldBe` [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]

    it "interleave" $ do
      interleave 1 [2, 3, 4] `shouldBe` [[1, 2, 3, 4], [2, 1, 3, 4], [2, 3, 1, 4], [2, 3, 4, 1]]

    it "permutations" $ do
      perms [1, 2, 3] `shouldBe` [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]
