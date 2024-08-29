module Chapter1  where

add_one :: Int -> Int
add_one n = n + 1

double :: Int -> Int
double n = 2 * n

quadruple :: Int -> Int
quadruple n = double (double n)

last' :: [a] -> a
last' xs = head (reverse xs)

last'' :: [a] -> a
last'' xs = (head . reverse) xs

last''' :: [a] -> a
last''' = head . reverse

last'''' :: [a] -> a
last'''' xs = xs!!(length xs - 1)
