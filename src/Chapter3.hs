module Chapter3 where

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0 .. n]

add' :: Int -> Int -> Int -> Int
add' x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: (Num a) => a -> a
double x = x * 2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

exclOr :: Bool -> Bool -> Bool
exclOr first second = first /= second

isSpace :: Char -> Bool
isSpace c = c == ' '

concatenateWithSpace :: String -> String -> String
concatenateWithSpace s1 s2 = s1 ++ " " ++ s2

squarePlus1 :: Int -> Int
squarePlus1 x = x ^ 2 + 1

threeUnequal :: Int -> Int -> Int -> Bool
threeUnequal x y z = x /= y && y /= z && x /= z

power :: Int -> Int -> Int
power base exponent = base ^ exponent
