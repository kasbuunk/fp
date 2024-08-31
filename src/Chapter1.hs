module Chapter1  where

import Playground

add_one :: Int -> Int
add_one n = n + 1

square :: Num a => a -> a
square side = side * side

double_twice :: Num a => a -> a
double_twice n = double' n
