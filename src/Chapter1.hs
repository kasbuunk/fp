module Chapter1 (add_one, double, quadruple) where

add_one :: Int -> Int
add_one n = n + 1

double :: Int -> Int
double n = 2 * n

quadruple :: Int -> Int
quadruple n = double (double n)
