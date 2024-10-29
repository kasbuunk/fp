module Chapter16 where

power :: Int -> Int -> Int
power x n
  | n == 0 = 1
  | otherwise = x * power x (n - 1)
