module PlaygroundSpec (spec) where

import Playground
import Test.Hspec

spec :: Spec
spec = do
  describe "Playground" $ do
    it "add one" $ do
      add_one 0 `shouldBe` 1

    it "double integers" $ do
      mapM_
        (\(input, expected) -> double input `shouldBe` expected)
        [ (0, 0),
          (2, 4),
          (-2, -4),
          (4, 8),
          (5, 10),
          (10, 20)
        ]

    it "quadruple integers" $ do
      mapM_
        (\(input, expected) -> quadruple input `shouldBe` expected)
        [ (3, 12),
          (5, 20)
        ]

    it "alternatively implement last" $ do
      let input = [1, 2, 3, 4, 5]
      let expected = last input

      last' input `shouldBe` expected
      last'' input `shouldBe` expected
      last''' input `shouldBe` expected
      last'''' input `shouldBe` expected

    it "select second from list" $ do
      second [1, 2, 3, 4, 5] `shouldBe` 2

    it "swap two items" $ do
      swap ('a', 5) `shouldBe` (5, 'a')

    it "form pair" $ do
      pair 1 "abc" `shouldBe` (1, "abc")

    it "double number" $ do
      double' 10.5 `shouldBe` 21

    it "is palindrome" $ do
      palindrome "aba"

    it "is not palindrome" $ do
      not (palindrome "ba")

    it "apply function twice" $ do
      quadruple 5 `shouldBe` twice double 5

    it "pipe operator" $ do
      reverse [1, 2, 3] |> head `shouldBe` head (reverse [1, 2, 3])

    it "faculty" $ do
      faculty 6 `shouldBe` 1 * 2 * 3 * 4 * 5 * 6
