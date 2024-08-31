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
