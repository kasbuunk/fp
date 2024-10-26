module Chapter15 where

multByPos :: [Int] -> [Int]
multByPos xs = [x * p | (x, p) <- zip xs [1 ..]]

multByPos' :: [Int] -> [Int]
multByPos' = zipWith (*) [1 ..]

powers :: Int -> [Int]
powers n = [n ^ x | x <- [0 ..]]

firstPowers :: Int -> Int -> [Int]
firstPowers amount = take amount . powers
