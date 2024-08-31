module Chapter1  where

import Playground

add_one :: Int -> Int
add_one n = n + 1

square :: Num a => a -> a
square side = side * side

double_twice :: Num a => a -> a
double_twice n = double' n

numbers :: Int -> [Int]
numbers n = [1..n]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
-- concatenate the smaller items with the current item x and the larger items.
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   -- All values of xs that are smaller than or equal to x.
                   smaller = [a | a <- xs, a <= x]
                   -- All values of xs that are greater than x.
                   larger = [a | a <- xs, a > x]
