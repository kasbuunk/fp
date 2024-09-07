module Chapter5 where

import Data.Char (isLower)

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
bin2dec bs = [binaryDecode b | b <- bs]

binaryDecode :: (Int, Int, Int) -> Int
binaryDecode (x, y, z) = sum [base * 2 ^ exponent | (base, exponent) <- zip bases exponents]
  where
    bases = [x, y, z]
    exponents = [2, 1, 0]
