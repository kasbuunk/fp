module Chapter15 where

multByPos :: [Int] -> [Int]
multByPos xs = [x * p | (x, p) <- zip xs [1 ..]]

multByPos' :: [Int] -> [Int]
multByPos' = zipWith (*) [1 ..]
