module Chapter7 where

add' :: Int -> Int -> Int
add' x y = x + y

twice :: (a -> a) -> a -> a
twice f a = f (f a)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x : xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x : xs)
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven xs = sum (map (^ 2) (filter even xs))

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

product' :: (Num a) => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

add1 :: Int -> Int
add1 = (+1)

double :: Int -> Int
double = (*2)
