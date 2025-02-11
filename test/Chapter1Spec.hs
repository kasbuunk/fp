module Chapter1Spec (spec) where

import Chapter1
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter1" $ do
    it "import test" $ do
      add_one 0 `shouldBe` 1

    -- W1.1 means exercise 1.1 in the work book.
    it "W1.1: surface square" $ do
      square 5 `shouldBe` 25

    it "T1.3a: generate list of numbers" $ do
      numbers 6 `shouldBe` [1, 2, 3, 4, 5, 6]

    it "T1.3b: numbers 5" $ do
      numbers 5 `shouldBe` [1, 2, 3, 4, 5]

    it "T1.3c: sum from 1 to 5" $ do
      sum (numbers 5) `shouldBe` 15

    it "Alternatively implement sum" $ do
      sum' (numbers 5) `shouldBe` sum (numbers 5)

    it "++ operator" $ do
      [] ++ [4] ++ [3, 2] ++ [] ++ [8] `shouldBe` [4, 3, 2, 8]

    it "Quicksort algorithm: list of chars" $ do
      mapM_
        (\(input, expected) -> qsort input `shouldBe` expected)
        [ ("abdc", "abcd"),
          ([], []),
          (['a'], ['a']),
          (['a', 'b'], ['a', 'b']),
          (['b', 'a'], ['a', 'b'])
        ]

    it "Quicksort algorithm: list of numbers" $ do
      mapM_
        (\(input, expected) -> qsort input `shouldBe` expected)
        [ ([], []),
          ([0], [0]),
          ([0, 3, 3, 2, 4, 1], [0, 1, 2, 3, 3, 4]),
          ([1, 4, 5, 2], [1, 2, 4, 5])
        ]

    it "T1.2: sum [x] = x" $ do
      mapM_
        (\(input, expected) -> sum input `shouldBe` expected)
        [ ([0], 0),
          ([1], 1),
          ([2], 2),
          ([2.5], 2.5)
        ]

    it "T1.3: define product alternatively" $ do
      mapM_
        (\(input, expected) -> product' input `shouldBe` expected)
        [ ([], 1),
          ([1], 1),
          ([2, 3, 4], 24)
        ]

    it "T1.4: reverse qsort" $ do
      mapM_
        (\(input, expected) -> reverse_qsort input `shouldBe` reverse (qsort input))
        [ ([], []),
          ([1], [1]),
          ([1, 2], [2, 1]),
          ([2, 4, 4, 3, 1, 9], [9, 4, 4, 3, 2, 1])
        ]

    it "T1.5: qsort remove duplicates" $ do
      qsort_unique [2, 2, 3, 1, 1] `shouldBe` [1, 2, 3]
