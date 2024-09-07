module Chapter5Spec where

import Chapter5
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter 5" $ do
    it "squares of integers 1 to 5" $ do
      [x ^ 2 | x <- [1 .. 5]] `shouldBe` [1, 4, 9, 16, 25]

    it "cartesian product" $ do
      cartesian [1, 2, 3] [4, 5] `shouldBe` [(1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)]

    it "second generator depends on first" $ do
      [(x, y) | x <- [1 .. 3], y <- [x .. 3]] `shouldBe` [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)]

    it "concatenate list of lists" $ do
      concatenate [[0, 1], [2, 3], [4]] `shouldBe` [0, 1, 2, 3, 4]

    it "first of pairs" $ do
      firsts [('a', 0), ('b', 1)] `shouldBe` ['a', 'b']

    it "alternatively calculate length of list" $ do
      length' [0, 1, 2] `shouldBe` 3

    it "even numbers up to n" $ do
      evensUpTo 10 `shouldBe` [0, 2, 4, 6, 8, 10]

    it "factors of n" $ do
      factors 15 `shouldBe` [1, 3, 5, 15]
      factors 7 `shouldBe` [1, 7]

    it "n is prime" $ do
      isPrime 6 `shouldBe` False
      isPrime 7 `shouldBe` True
      isPrime 100 `shouldBe` False
      isPrime 101 `shouldBe` True

    it "all primes up to n" $ do
      primes 7 `shouldBe` [2, 3, 5, 7]
      primes 20 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19]

    it "look up value by key" $ do
      find 0 [(0, 'a'), (1, 'b')] `shouldBe` ['a']
      find 1 [(0, 'a'), (1, 'b')] `shouldBe` ['b']
      find 1 [(0, 'a'), (1, 'b'), (1, 'c')] `shouldBe` ['b', 'c']

    it "try zip" $ do
      zip ['a', 'b', 'c', 'd'] [0, 1, 2, 3, 4, 5] `shouldBe` [('a', 0), ('b', 1), ('c', 2), ('d', 3)]

    it "pairs adjacent elements" $ do
      pairs [0, 1, 2, 3] `shouldBe` [(0, 1), (1, 2), (2, 3)]

    it "is list sorted" $ do
      sorted [0, 1, 2, 3] `shouldBe` True
      sorted [0, 5, 1, 2, 3] `shouldBe` False

    it "positions in list" $ do
      positions False [True, False, True, False] `shouldBe` [1, 3]

    it "string comprehensions" $ do
      "abcde" !! 3 `shouldBe` 'd'
      take 3 "abcde" `shouldBe` "abc"
      length "abcde" `shouldBe` 5
      zip "abc" [1, 2, 3, 4] `shouldBe` [('a', 1), ('b', 2), ('c', 3)]

    it "count lower case characters in string" $ do
      lowers "ABCDE" `shouldBe` 0
      lowers "abCdE" `shouldBe` 3
      lowers "abcde" `shouldBe` 5

    it "count chars in string" $ do
      count 'a' "abcde" `shouldBe` 1
      count 'a' "abcdae" `shouldBe` 2
      count 'x' "abcdae" `shouldBe` 0

    it "all integers are even" $ do
      allEven [0, 2, 2, 4, 6] `shouldBe` True
      allEven [1, 0, 2, 2, 4, 6] `shouldBe` False

    it "binary encoded integers" $ do
      codes () `shouldBe` [(0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1), (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1)]

      binaryDecode (0, 0, 0) `shouldBe` 0
      binaryDecode (1, 1, 1) `shouldBe` 7

      bin2dec [(0, 0, 0)] `shouldBe` [0]
      bin2dec [(0, 0, 1)] `shouldBe` [1]

      bin2dec (codes ()) `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7]
