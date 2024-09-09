module Chapter6 where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

recursiveMultiply :: Int -> Int -> Int
recursiveMultiply _ 0 = 0
recursiveMultiply x y = x + recursiveMultiply x (y - 1)
