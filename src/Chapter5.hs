module Chapter5 where

import Chapter1 (square)
import Data.Char (chr, digitToInt, isLower, ord)
import Data.List (sortBy)
import Data.Ord (comparing)

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]

concatenate :: [[a]] -> [a]
concatenate xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts xs = [x | (x, _) <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

evensUpTo :: Int -> [Int]
evensUpTo n = [x | x <- [0 .. n], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x]

find :: (Eq a) => a -> [(a, b)] -> [b]
find key dictionary = [value | (drawnKey, value) <- dictionary, drawnKey == key]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- sorted returns whether a list is sorted. It checks this by defining all
-- adjacent pairs of elements and for each pair, checks that they're ordered.
sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: (Eq a) => a -> [a] -> [Int]
positions needle xs = [index | (x, index) <- zip xs [0 ..], x == needle]

lowers :: [Char] -> Int
lowers cs = length [c | c <- cs, isLower c]

count :: Char -> [Char] -> Int
count needle cs = length [c | c <- cs, c == needle]

allEven :: [Int] -> Bool
allEven xs = and [even x | x <- xs]

codes :: () -> [(Int, Int, Int)]
codes _ = [(x, y, z) | x <- [0, 1], y <- [0, 1], z <- [0, 1]]

bin2dec :: [(Int, Int, Int)] -> [Int]
bin2dec bs = [binaryDecode [x, y, z] | (x, y, z) <- bs]

intsDownFrom :: Int -> [Int]
intsDownFrom n = reverse [0 .. n]

binaryDecode :: [Int] -> Int
binaryDecode weights = sum [weight * 2 ^ e | (weight, e) <- zip weights (exponents weights)]

binaryDecode3Tuple :: (Int, Int, Int) -> Int
binaryDecode3Tuple (x, y, z) = binaryDecode [x, y, z]

between :: Int -> Int -> [Int]
between lower upper = [lower + 1 .. upper - 1]

occurrences :: (Eq a) => a -> [a] -> [a]
occurrences needle xs = [x | x <- xs, x == needle]

isIn :: (Eq a) => a -> [a] -> Bool
isIn needle xs = length (occurrences needle xs) > 0

evenElements :: [a] -> [a]
evenElements xs = [x | (x, index) <- zip xs [0 ..], even (index + 1)]

selectEvenLT1 :: [Int] -> [Float] -> [(Int, Float)]
selectEvenLT1 numbers prices = [(n, p) | (n, p) <- zip numbers prices, even n, p < 1.0]

exponents :: [a] -> [Int]
exponents xs = intsDownFrom (length xs - 1)

stringToInt :: String -> Int
stringToInt weights = sum [digitToInt weight * 10 ^ e | (weight, e) <- zip weights (exponents weights)]

alphaToInt :: Char -> Int
alphaToInt c = ord c - ord 'a'

intToAlpha :: Int -> Char
intToAlpha n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c
  | isLower c = intToAlpha ((alphaToInt c + n) `mod` 26)
  | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n cs = [shift n c | c <- cs]

frequencyTable :: () -> [Float]
frequencyTable _ =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n) / (fromIntegral m) * 100.0

reasonablyClose :: Float -> Float -> Bool
reasonablyClose n m = (n - m) / m <= 0.001

frequencies :: [Char] -> [Float]
frequencies cs = [percent (count c cs) n | c <- ['a' .. 'z']]
  where
    n = lowers cs

chiSquared :: [Float] -> [Float] -> Float
chiSquared os es = sum [(o - e) ^ 2 / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n' xs ++ take n' xs
  where
    n' = n `mod` length xs

sortByFirst :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFirst = sortBy (comparing fst)

crack :: String -> String
crack input = encode (-cipherGuess) input
  where
    cipherGuess = head (positions (minimum cipherResults) cipherResults)
    cipherResults = [chiSquared (rotate n os) es | n <- [0 .. 25]]
    os = frequencies input
    es = frequencyTable ()

sumSquaresUpTo :: Int -> Int
sumSquaresUpTo n = sum [x ^ 2 | x <- [1 .. n]]

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(n', m') | n' <- [0 .. n], m' <- [0 .. m]]

squareGrid :: Int -> [(Int, Int)]
squareGrid n = [(n', m') | (n', m') <- grid n n, n' /= m']

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [0 .. n - 1]]
