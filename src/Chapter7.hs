module Chapter7 where

import Data.Char

add' :: Int -> Int -> Int
add' x y = x + y

twice :: (a -> a) -> a -> a
twice f = f . f

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
sumsqreven = sum . map (^ 2) . filter even

sum' :: (Num a) => [a] -> a
sum' = foldr' (+) 0

product' :: (Num a) => [a] -> a
product' = foldr' (*) 1

or' :: [Bool] -> Bool
or' = foldr' (||) False

and' :: [Bool] -> Bool
and' = foldr' (&&) True

add1 :: Int -> Int
add1 = (+ 1)

double :: Int -> Int
double = (* 2)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

length'' :: [a] -> Int
length'' = foldl (\acc _ -> acc + 1) 0

reverse' :: [a] -> [a]
reverse' = foldr snoc []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

sum'' :: (Num a) => [a] -> a
sum'' = sum''' 0
  where
    sum''' v [] = v
    sum''' v (x : xs) = x + sum''' v xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

odd' :: (Integral a) => a -> Bool
odd' = not . even

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits
  | length bits < 8 = make8 (bits ++ [0])
  | otherwise = bits

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: a -> a
channel = id

count :: (Eq a) => a -> [a] -> Int
count needle = length . filter (== needle)

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : filter (/= x) (removeDuplicates xs)

result :: Ord a => [a] -> [(Int, a)]
result votes' = sortByFirst [(count uniqueVote votes', uniqueVote) | uniqueVote <- uniqueVotes]
  where
    uniqueVotes = removeDuplicates votes'

sortByFirst :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortByFirst [] = []
sortByFirst (x : xs) = sortByFirst smaller ++ [x] ++ sortByFirst greater
  where
    smaller = [x' | x' <- xs, x' <= x]
    greater = [x' | x' <- xs, x' > x]

winner :: [(a, b)] -> b
winner = snd . last

rmempty :: (Eq a) => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: (Eq a) => a -> [[a]] -> [[a]]
elim target = map (filter (/= target))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head
