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

    it "find all" $ do
      let t = [("foo", 0), ("bar", 1), ("foo", 7), ("baz", 10)]
       in findAll "foo" t `shouldBe` [0, 7]

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

    it "tautology checker: const" $ do
      tautology (Const True) `shouldBe` True
      tautology (Const False) `shouldBe` False

    it "tautology checker: and, not" $ do
      tautology (And (Var 'A') (Not (Var 'A'))) `shouldBe` False

    it "tautology checker: implication" $ do
      tautology (Imply (And (Var 'A') (Var 'B')) (Var 'A')) `shouldBe` True
      tautology (Imply (Var 'A') (And (Var 'B') (Var 'A'))) `shouldBe` False
      tautology (Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')) `shouldBe` True

    it "tautology checker: disjunction" $ do
      tautology (Or (Var 'A') (Const True)) `shouldBe` True
      tautology (Or (Var 'A') (Const False)) `shouldBe` False

    it "tautology checker: equivalence" $ do
      tautology (Iff (Var 'A') (Var 'A')) `shouldBe` True
      tautology (Iff (Var 'A') (Const False)) `shouldBe` False

    it "generate boolean combinations" $ do
      bools 0 `shouldBe` ([] :: [[Bool]])
      bools 1 `shouldBe` [[False], [True]]
      bools 2 `shouldBe` [[False, False], [False, True], [True, False], [True, True]]

    it "generate substitutions for proposition" $ do
      substitutes (Imply (Var 'A') (And (Var 'B') (Var 'A')))
        `shouldBe` [ [('A', False), ('B', False)],
                     [('A', False), ('B', True)],
                     [('A', True), ('B', False)],
                     [('A', True), ('B', True)]
                   ]

    it "abstract machine" $ do
      value (Val 0) `shouldBe` 0
      value (Add (Val 2) (Val 3)) `shouldBe` 5
      value (Add (Add (Val 2) (Val 3)) (Val 4)) `shouldBe` 9
      value' (Val 0) `shouldBe` 0
      value' (Add (Val 2) (Val 3)) `shouldBe` 5
      value' (Add (Add (Val 2) (Val 3)) (Val 4)) `shouldBe` 9

    it "abstract machine: multiplication" $ do
      value' (Add (Mult (Val 2) (Val 3)) (Val 4)) `shouldBe` 10

    it "int <-> nat" $ do
      nat2int (int2nat 0) `shouldBe` 0
      nat2int (int2nat 5) `shouldBe` 5

    it "multiply natural numbers" $ do
      nat2int (mult Zero Zero) `shouldBe` 0
      nat2int (mult Zero (Succ Zero)) `shouldBe` 0
      nat2int (mult (Succ Zero) Zero) `shouldBe` 0
      nat2int (mult (Succ Zero) (Succ Zero)) `shouldBe` 1
      nat2int (mult (int2nat 2) (int2nat 3)) `shouldBe` 6
      nat2int (mult (int2nat 5) (int2nat 3)) `shouldBe` 15

    it "occurs in tree" $ do
      occurs 5 (Leaf 5) `shouldBe` True
      occurs 5 (Leaf 10) `shouldBe` False
      occurs 5 (Node (Leaf 2) 3 (Leaf 4)) `shouldBe` False
      occurs 5 (Node (Leaf 2) 3 (Leaf 5)) `shouldBe` True
      occurs' 5 (Leaf 5) `shouldBe` True
      occurs' 5 (Leaf 10) `shouldBe` False
      occurs' 1 (Node (Leaf 2) 3 (Leaf 4)) `shouldBe` False
      occurs' 2 (Node (Leaf 2) 3 (Leaf 4)) `shouldBe` True
      occurs' 3 (Node (Leaf 2) 3 (Leaf 4)) `shouldBe` True
      occurs' 4 (Node (Leaf 2) 3 (Leaf 4)) `shouldBe` True
      occurs' 5 (Node (Leaf 2) 3 (Leaf 4)) `shouldBe` False

    it "balanced tree" $ do
      balanced (Node' (Node' (Leaf' 1) (Leaf' 2)) (Leaf' 1)) `shouldBe` True
      balanced (Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))) `shouldBe` True
      balanced (Node' (Node' (Node' (Leaf' 5) (Leaf' 8)) (Leaf' 2)) (Leaf' 1)) `shouldBe` False

    it "balance tree" $ do
      balanced (balance [1]) `shouldBe` True
      balanced (balance [1, 2]) `shouldBe` True
      balanced (balance [1, 2, 3]) `shouldBe` True
      balanced (balance [1, 2, 3, 4]) `shouldBe` True
      balanced (balance [1, 2, 3, 4, 5]) `shouldBe` True

    it "folde" $ do
      eval' (Add (Add (Val 2) (Val 4)) (Val 1)) `shouldBe` 7

    it "testNumber" $ do
      testNumber 10 18 `shouldBe` Nothing
      testNumber 10 8 `shouldBe` Just 8
      testNumber 10 (-1) `shouldBe` Nothing

    it "linked list" $ do
      list2datalist (datalist2list [1]) `shouldBe` [1]
      list2datalist (datalist2list [1, 2, 3]) `shouldBe` [1, 2, 3]
