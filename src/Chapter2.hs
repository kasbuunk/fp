module Chapter2 where

add_one x = x+1

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]

average :: [Int] -> Int
average [] = 0
average xs = sum xs `div` length xs

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


init' :: [a] -> [a]
init' [] = []
init' xs = reverse (tail (reverse xs))
