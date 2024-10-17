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

    it "choices" $ do
      choices [1, 2, 3] `shouldBe` [[], [3], [2], [2, 3], [3, 2], [1], [1, 3], [3, 1], [1, 2], [2, 1], [1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]
      choices' [1, 2, 3] `shouldBe` [[], [3], [2], [2, 3], [3, 2], [1], [1, 3], [3, 1], [1, 2], [2, 1], [1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]

    it "solution" $ do
      let e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
       in solution e [1, 3, 7, 10, 25, 50] 765

    it "split" $ do
      split [1, 2, 3, 4] `shouldBe` [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]

    it "solutions" $ do
      -- length (solutions [1, 3, 7, 10, 25, 50] 765) `shouldBe` 780 -- Slow
      length (solutions' [1, 3, 7, 10, 25, 50] 765) `shouldBe` 780
      length (solutions'' [1, 3, 7, 10, 25, 50] 765) `shouldBe` 49

    it "is choice" $ do
      isChoice [1, 2] [1, 2, 3] `shouldBe` True
      isChoice [1, 4] [1, 2, 3] `shouldBe` False

    it "count expressions" $ do
      let ns = [1, 3, 7, 10, 25, 50] :: [Int]
       in length (concatMap exprs (choices ns)) `shouldBe` 33665406

    -- Slow
    -- it "count valid expressions" $ do
    --   let ns = [1, 3, 7, 10, 25, 50] :: [Int]
    --    in length (filter (\x -> length x == 1) (map eval (concatMap exprs (choices ns)))) `shouldBe` 4672540

    -- Slow
    -- it "count valid expressions for all integers" $ do
    --   let ns = [1, 3, 7, 10, 25, 50] :: [Int]
    --    in length (filter (\x -> length x == 1) (map eval' (concatMap exprs (choices ns)))) `shouldBe` 10839369

    it "alternative subs" $ do
      subs' [1] `shouldBe` [[], [1]]
      subs' [1, 2, 3] `shouldBe` [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]
