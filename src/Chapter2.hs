module Chapter2 where

add_one x = x+1

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]
