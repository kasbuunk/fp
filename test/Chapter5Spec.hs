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
