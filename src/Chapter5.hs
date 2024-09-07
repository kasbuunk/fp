module Chapter5 where

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
