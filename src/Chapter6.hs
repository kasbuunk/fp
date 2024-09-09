module Chapter6 where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

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
