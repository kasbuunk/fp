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
      find' 0 [(0, 'a'), (1, 'b')] `shouldBe` ['a']
      find' 1 [(0, 'a'), (1, 'b')] `shouldBe` ['b']
      find' 1 [(0, 'a'), (1, 'b'), (1, 'c')] `shouldBe` ['b', 'c']

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

      binaryDecode3Tuple (0, 0, 0) `shouldBe` 0
      binaryDecode3Tuple (1, 1, 1) `shouldBe` 7

      bin2dec [(0, 0, 0)] `shouldBe` [0]
      bin2dec [(0, 0, 1)] `shouldBe` [1]

      bin2dec (codes ()) `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7]

    it "integers from n down to 0" $ do
      intsDownFrom 5 `shouldBe` [5, 4, 3, 2, 1, 0]

    it "numbers between lower and upper bound" $ do
      between 5 10 `shouldBe` [6, 7, 8, 9]

    it "occurrences in list" $ do
      occurrences 10 [1, 2, 3, 1] `shouldBe` []
      occurrences 1 [1, 2, 3, 1] `shouldBe` [1, 1]
      isIn 1 [1, 2, 3, 1] `shouldBe` True
      isIn 0 [1, 2, 3, 1] `shouldBe` False

    it "elements at even positions" $ do
      evenElements ["aap", "noot", "mies"] `shouldBe` ["noot"]
      evenElements [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]
      evenElements [0, 1, 2, 3, 4, 5, 6] `shouldBe` [1, 3, 5]

    it "select product" $ do
      let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      let prices = [0.1, 0.1, 0.1, 0.1, 0.1, 2.0, 0.1, 2.0, 0.1, 2.0]
      selectEvenLT1 numbers prices `shouldBe` [(2, 0.1), (4, 0.1)]

    it "string to int" $ do
      stringToInt "123" `shouldBe` 123

    it "encode and decode between char and int" $ do
      alphaToInt 'a' `shouldBe` 0
      alphaToInt 'z' `shouldBe` 25
      intToAlpha 0 `shouldBe` 'a'
      intToAlpha 25 `shouldBe` 'z'

    it "shift letter" $ do
      shift 1 'a' `shouldBe` 'b'
      shift (-1) 'a' `shouldBe` 'z'
      shift 1 'z' `shouldBe` 'a'
      shift 1 ',' `shouldBe` ','

    it "shift string" $ do
      encode 1 "abc" `shouldBe` "bcd"
      encode (-1) "abc" `shouldBe` "zab"
      encode (-5) (encode 5 "haskell is interesting") `shouldBe` "haskell is interesting"

    it "check table length" $ do
      length (frequencyTable ()) `shouldBe` 26

    it "calculate percentage" $ do
      reasonablyClose (percent 5 15) 33.33333 `shouldBe` True

    it "fequencies of characters" $ do
      frequencies "ab"
        `shouldBe` [ 50.0,
                     50.0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0
                   ]

    it "chi squared" $ do
      let observed = [1.0, 0.1]
      let expected = [1.0, 0.1]
      chiSquared observed expected `shouldBe` 0.0

      let observed' = [1.0, 2.0]
      let expected' = [2.0, 4.0]
      chiSquared observed' expected' `shouldBe` (0.5 + 1.0)

    it "rotate string" $ do
      rotate 0 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      rotate 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5, 1, 2, 3]
      rotate 5 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      rotate 8 [1, 2, 3, 4, 5] `shouldBe` [4, 5, 1, 2, 3]

    it "crack caesar" $ do
      let text = "the sun was setting behind the hills, casting a warm orange glow over the small town. people were finishing their day, heading home from work or sitting outside their houses, enjoying the evening breeze. the streets were quiet, and the occasional sound of laughter or distant music echoed in the air. birds flew across the sky in graceful patterns, making their way back to their nests. it was one of those moments where everything felt peaceful and calm. as night began to fall, the town lights flickered on, casting a soft glow on the empty streets. the air grew cooler, and a few stars started to twinkle in the darkening sky. some families gathered around their dinner tables, while others took evening strolls, talking quietly as they walked. the town seemed to slow down, preparing for another restful night."
      let cipher = 4

      crack (encode cipher text) `shouldBe` text

  it "sum of squared numbers until n" $ do
    sumSquaresUpTo 1 `shouldBe` 1
    sumSquaresUpTo 2 `shouldBe` 5
    sumSquaresUpTo 100 `shouldBe` 338350

  it "grid m x n" $ do
    grid 1 2 `shouldBe` [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]

  it "square grid excluding diagonal" $ do
    squareGrid 2 `shouldBe` [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]

  it "implement replicate with list comprehensions" $ do
    replicate' 3 True `shouldBe` [True, True, True]

  it "pythagorean triples" $ do
    pythagoreanTriples 10 `shouldBe` [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

  it "perfect numbers" $ do
    perfects 500 `shouldBe` [6, 28, 496]

  it "redefine positions" $ do
    positions' 'a' ['b', 'a', 'b', 'a'] `shouldBe` [1, 3]

  it "scalar product" $ do
    scalarproduct [1 .. 3] [4 .. 6] `shouldBe` 32
