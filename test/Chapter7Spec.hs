module Chapter7Spec where

import Chapter7
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter7" $ do
    it "apply twice" $ do
      let add1 = add' 1
      twice add1 0 `shouldBe` 2
      twice reverse [1, 2, 3] `shouldBe` [1, 2, 3]
      twice (2 *) 5 `shouldBe` 20
      twice (* 2) 5 `shouldBe` 20

    it "redefine map" $ do
      map' (+ 1) [0, 1, 2] `shouldBe` [1, 2, 3]
      map' even [0, 1, 2] `shouldBe` [True, False, True]
      map' reverse ["abc", "def", "ghi"] `shouldBe` ["cba", "fed", "ihg"]
      map' (map' (+ 1)) [[0, 1], [2, 3]] `shouldBe` [[1, 2], [3, 4]]

    it "redefine map recursively" $ do
      map'' (+ 1) [0, 1, 2] `shouldBe` [1, 2, 3]
      map'' even [0, 1, 2] `shouldBe` [True, False, True]
      map'' reverse ["abc", "def", "ghi"] `shouldBe` ["cba", "fed", "ihg"]
      map'' (map' (+ 1)) [[0, 1], [2, 3]] `shouldBe` [[1, 2], [3, 4]]

    it "redefine filter" $ do
      filter' (\_ -> False) [0, 1, 2, 3] `shouldBe` []
      filter' (\_ -> True) [0, 1, 2, 3] `shouldBe` [0, 1, 2, 3]
      filter' (\x -> x > 1) [0, 1, 2, 3] `shouldBe` [2, 3]
      filter' (\x -> x == 1) [0, 1, 2, 3] `shouldBe` [1]
      filter' even [0 .. 10] `shouldBe` [0, 2, 4, 6, 8, 10]
      filter' (> 2) [0 .. 5] `shouldBe` [3, 4, 5]
      filter' (/= ' ') "this sentence without spaces" `shouldBe` "thissentencewithoutspaces"

    it "redefine filter recursively" $ do
      filter'' even [0 .. 10] `shouldBe` [0, 2, 4, 6, 8, 10]

    it "sum of squares is even" $ do
      sumsqreven [0, 1, 3] `shouldBe` 0
      sumsqreven [0 .. 6] `shouldBe` 56

    it "all with predicate" $ do
      all even [0, 2, 6, 4, 8] `shouldBe` True

    it "any with predicate" $ do
      any odd [0, 1, 2] `shouldBe` True
      any odd [0, 10, 2] `shouldBe` False

    it "takeWhile with predicate" $ do
      takeWhile even [0, 2, 10, 9, 2, 3, 0] `shouldBe` [0, 2, 10]

    it "dropWhile with predicate" $ do
      dropWhile odd [1, 7, 3, 0, 2, 10, 7] `shouldBe` [0, 2, 10, 7]

    it "redefine with foldr" $ do
      sum' [] `shouldBe` 0
      sum' [5 .. 7] `shouldBe` 18

      product' [] `shouldBe` 1
      product' [5 .. 7] `shouldBe` 210

      or' [] `shouldBe` False
      or' [False, True] `shouldBe` True

      and' [] `shouldBe` True
      and' [False, True] `shouldBe` False

    it "composition" $ do
      (double . add1) 5 `shouldBe` 12
      (add1 . double) 5 `shouldBe` 11

    it "length with foldr" $ do
      length' "" `shouldBe` 0
      length' "abcdef" `shouldBe` 6

    it "reverse with foldr" $ do
      reverse' "" `shouldBe` ""
      reverse' "abcdef" `shouldBe` "fedcba"

    it "length with foldl" $ do
      length'' "" `shouldBe` 0
      length'' "abcdef" `shouldBe` 6

    it "reverse with foldl" $ do
      reverse'' "" `shouldBe` ""
      reverse'' "abcdef" `shouldBe` "fedcba"

    it "sum with foldl" $ do
      sum'' [] `shouldBe` 0
      sum'' [5, 10, 12] `shouldBe` 27

    it "exponent with foldl" $ do
      foldl' (^) 2 [3, 4] `shouldBe` ((2 ^ 3) ^ 4)

    it "exponent with foldr" $ do
      foldr (^) 2 [3, 4] `shouldBe` (3 ^ (4 ^ 2))

    it "compose odd" $ do
      odd' 1 `shouldBe` True
      odd' 2 `shouldBe` False

    it "compose list" $ do
      compose [not] True `shouldBe` False
      compose [not, not] False `shouldBe` False
      compose [not, not, not] True `shouldBe` False
      compose [not, not, not] False `shouldBe` True

    it "binary to int" $ do
      bin2int [1, 0, 1, 1] `shouldBe` 13

    it "int to binary" $ do
      int2bin 13 `shouldBe` [1, 0, 1, 1]

    it "make 8 bits" $ do
      make8 [1, 0, 1, 1] `shouldBe` [1, 0, 1, 1, 0, 0, 0, 0]

    it "encode string to bits" $ do
      encode "abc" `shouldBe` [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0]
      decode [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0] `shouldBe` "abc"
      decode (encode "abc") `shouldBe` "abc"

    it "encode char to bits without parity" $ do
      let encodedA = [1, 0, 0, 0, 0, 1, 1, 0]
      let encodedB = [0, 1, 0, 0, 0, 1, 1, 0]
      let encodedC = [1, 1, 0, 0, 0, 1, 1, 0]

      encodeChar 'a' `shouldBe` encodedA
      encodeChar 'b' `shouldBe` encodedB
      encodeChar 'c' `shouldBe` encodedC

    it "encode char to bits with parity" $ do
      let encodedA = [1, 0, 0, 0, 0, 1, 1, 0]
      let encodedB = [0, 1, 0, 0, 0, 1, 1, 0]
      let encodedC = [1, 1, 0, 0, 0, 1, 1, 0]

      let encodedAWithParity = encodedA ++ [1]
      let encodedBWithParity = encodedB ++ [1]
      let encodedCWithParity = encodedC ++ [0]

      encodeCharWithParity 'a' `shouldBe` encodedAWithParity
      encodeCharWithParity 'b' `shouldBe` encodedBWithParity
      encodeCharWithParity 'c' `shouldBe` encodedCWithParity

      decodeWithParity (encodeWithParity "abc") `shouldBe` "abc"

    it "expect parity check error" $ do
      evaluate
        ( let encodedA = [1, 0, 0, 0, 0, 1, 1, 0]
           in parityCheck (encodedA ++ [0])
        )
        `shouldThrow` anyErrorCall

    it "expect parity check error whilst decoding char" $ do
      evaluate
        ( let encodedC = [1, 1, 0, 0, 0, 1, 1, 0]
           in decodeCharWithParity (encodedC ++ [1])
        )
        `shouldThrow` anyErrorCall

    it "expect parity check error whilst decoding string" $ do
      evaluate
        ( let encodedA = [1, 0, 0, 0, 0, 1, 1, 0]
              encodedB = [0, 1, 0, 0, 0, 1, 1, 0]
              encodedC = [1, 1, 0, 0, 0, 1, 1, 0]

              encodedAWithParity = encodedA ++ [1]
              encodedBWithParity = encodedB ++ [0] -- Parity check error!
              encodedCWithParity = encodedC ++ [0]

              cipherText = encodedAWithParity ++ encodedBWithParity ++ encodedCWithParity
           in decodeCharWithParity (cipherText)
        )
        `shouldThrow` anyErrorCall

    it "transmit string" $ do
      transmit "abcde" `shouldBe` "abcde"

    it "transmit string with parity" $ do
      transmitWithParity "abcde" `shouldBe` "abcde"

    it "faulty transmitter" $ do
      evaluate (force (transmitFaulty "abcde")) `shouldThrow` anyErrorCall

    it "count elements" $ do
      count "Green" [] `shouldBe` 0
      count "Green" ["Red", "Green", "Green", "Blue", "Green"] `shouldBe` 3

    it "remove duplicates" $ do
      removeDuplicates votes `shouldBe` ["Red", "Blue", "Green"]

    it "first past the post" $ do
      result votes `shouldBe` [(1, "Green"), (2, "Red"), (3, "Blue")]
      winner (result votes) `shouldBe` "Blue"

    it "remove empty ballot" $ do
      rmempty [[], ["Green"]] `shouldBe` [["Green"]]
      rmempty [["Green"], [], ["Red", "Blue"]] `shouldBe` [["Green"], ["Red", "Blue"]]

    it "eliminate losers" $ do
      let ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]
       in elim "Red" ballots `shouldBe` [["Green"], ["Blue"], ["Green", "Blue"], ["Blue", "Green"], ["Green"]]

    it "rank ballot results" $ do
      let ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]
       in rank ballots `shouldBe` ["Red", "Blue", "Green"]

    it "winner of ballot votes" $ do
      let ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]
       in winner' ballots `shouldBe` "Green"

    it "re-express list comprehension" $ do
      filterMap (>= 1) (\x -> x + 1) [0, 2, 3, -1] `shouldBe` [3, 4]
      filterMap' (>= 1) (\x -> x + 1) [0, 2, 3, -1] `shouldBe` [3, 4]

    it "redefine all" $ do
      all' (> 0) [1, 2, 3] `shouldBe` True
      all' (> 0) [1, 2, 3, 0] `shouldBe` False

    it "redefine any" $ do
      any' (\x -> length x > 10) ["", "short", "not long"] `shouldBe` False
      any' (\x -> length x > 10) ["", "short", "surely long enough"] `shouldBe` True

    it "redefine dropWhile" $ do
      dropWhile' (> 0) [1, 2, 3, -1, 3] `shouldBe` [-1, 3]
      dropWhile' (> 0) [0, 1, 2, 3, -1, 3] `shouldBe` [0, 1, 2, 3, -1, 3]

    it "redefine takeWhile" $ do
      takeWhile' (> 0) [1, 2, 3, -1, 3] `shouldBe` [1, 2, 3]
      takeWhile' (> 0) [0, 1, 2, 3, -1, 3] `shouldBe` []

    it "redefine map f" $ do
      map''' (+ 1) [0, 1, 2] `shouldBe` [1, 2, 3]
      map''' (++ ['a']) ["", "x", "abc"] `shouldBe` ["a", "xa", "abca"]
      map'''' (+ 1) [0, 1, 2] `shouldBe` [1, 2, 3]
      map'''' (++ ['a']) ["", "x", "abc"] `shouldBe` ["a", "xa", "abca"]

    it "redefine filter p" $ do
      filter''' (== 1) [0, 1, 2, 3, 1, 1] `shouldBe` [1, 1, 1]
      filter''' (/= 1) [0, 1, 2, 3, 1, 1] `shouldBe` [0, 2, 3]

    it "dec2int" $ do
      dec2int [2, 3, 4, 5] `shouldBe` 2345

    it "redefine uncurry" $ do
      uncurry' (\x -> \y -> x + y) (1, 2) `shouldBe` 3

    it "redefine curry" $ do
      curry' (uncurry' (\x -> \y -> x + y)) 1 2 `shouldBe` 3

    it "redefine chop8 with unfold" $ do
      let bitsInput = [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1]
      let bitsOutput = [[0, 0, 0, 0, 1, 1, 1, 1], [0, 0, 1, 1, 0, 0, 1, 1], [0, 1, 0, 1, 0, 1, 0, 1]]
      chop8 bitsInput `shouldBe` bitsOutput
      chop8' bitsInput `shouldBe` bitsOutput

    it "redefine iterate with unfold" $ do
      let xs = [xs | (_, xs) <- zip [0, 0, 0] (iterate' (+ 1) 0)]
      let ys = [ys | (_, ys) <- zip [0, 0, 0] (iterate' (* 2) 1)]
      xs `shouldBe` [0, 1, 2]
      ys `shouldBe` [1, 2, 4]

    it "alt map" $ do
      altMap (+ 10) (+ 100) [0 .. 4] `shouldBe` [10, 101, 12, 103, 14]

    it "luhn algorithm" $ do
      luhn [1, 7, 8, 4] `shouldBe` True
      luhn [4, 7, 8, 3] `shouldBe` False
