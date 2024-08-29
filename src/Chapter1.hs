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

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double' :: Num a => a -> a
double' x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
