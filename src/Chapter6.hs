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

-- length' [1,2,3]
-- = 1 + length' [2,3]
-- = 1 + 1 + length' [3]
-- = 1 + 1 + 1 + length' []
-- = 1 + 1 + 1 + 0
-- = 3
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

-- drop' 3 [1,2,3,4,5]
-- = drop' 2 [2,3,4,5]
-- = drop' 1 [3,4,5]
-- = drop' 0 [4,5]
-- = [4,5]
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

-- e.g.
-- exp' 2 3
-- = 2 * exp' 2 2
-- = 2 * 2 * exp' 2 1
-- = 2 * 2 * 2 * exp' 2 0
-- = 2 * 2 * 2 * 1
-- = 8
exp' :: (Num a, Eq a, Integral b) => a -> b -> a
exp' _ 0 = 1
exp' 0 _ = 0
exp' x y = x * exp' x (y - 1)

euclid :: Int -> Int -> Int
euclid x y
  | x == 0 || y == 0 = 0
  | x == y = x
  | x < y = euclid x (y - x)
  | otherwise = euclid (x - y) y

-- init' [1,2,3]
-- = 1 : init' [2,3]
-- = 1 : 2 : init' [3]
-- = 1 : 2 : []
-- = [1,2]
init' :: [a] -> [a]
init' [] = undefined
init' [_] = []
init' (x : xs) = x : init' xs

and' :: [Bool] -> Bool
and' [] = True
and' (False : _) = False
and' (True : xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[xs]] = [xs]
concat' (xs : xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n - 1) x

select :: [a] -> Int -> a
select [] _ = undefined
select (x : _) 0 = x
select (_ : xs) n = select xs (n - 1)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (x' : xs)
  | x == x' = True
  | otherwise = elem' x xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys
