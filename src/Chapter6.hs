module Chapter6 where

fac :: Int -> Int
fac n
  | n < 0 = undefined
  | n == 0 = 1
  | otherwise = n * fac (n - 1)

recursiveMultiply :: Int -> Int -> Int
recursiveMultiply _ 0 = 0
recursiveMultiply x y = x + recursiveMultiply x (y - 1)

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x : xs) = x * product xs

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort (x : xs) = insert x (insertionsort xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (_ : xs) = drop (n - 1) xs
drop' _ _ = []

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lower ++ [x] ++ qsort greater
  where
    lower = [y | y <- xs, y <= x]
    greater = [z | z <- xs, z > x]

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

exp' :: (Num a, Eq a, Integral b) => a -> b -> a
exp' _ 0 = 1
exp' 0 _ = 0
exp' x y = x * exp' x (y - 1)
