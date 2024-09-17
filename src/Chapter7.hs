module Chapter7 where

import Chapter4 (luhnDouble)
import Data.Char
import Data.List

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

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ acc [] = acc
foldl'' f acc (x : xs) = foldl'' f (f acc x) xs

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
encode = concat . map encodeChar

encodeChar :: Char -> [Bit]
encodeChar = make8 . int2bin . ord

encodeCharWithParity :: Char -> [Bit]
encodeCharWithParity = appendParityBit . make8 . int2bin . ord

encodeWithParity :: String -> [Bit]
encodeWithParity = concat . map (appendParityBit . encodeChar)

decodeWithParity :: [Bit] -> String
decodeWithParity = map decodeCharWithParity . chop 9

decodeCharWithParity :: [Bit] -> Char
decodeCharWithParity = chr . bin2int . parityCheck

parityCheck :: [Bit] -> [Bit]
parityCheck bits
  | length bits == 9 && even (sum bits) = take 8 bits
  | otherwise = error "Incorrect parity check"

appendParityBit :: [Bit] -> [Bit]
appendParityBit bits = bits ++ if even (count 1 bits) then [0] else [1]

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

chop8 :: [Bit] -> [[Bit]]
chop8 = chop 8

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

transmitWithParity :: String -> String
transmitWithParity = decodeWithParity . channel . encodeWithParity

-- transmitFaulty forgets the first bit during transmission.
transmitFaulty :: String -> String
transmitFaulty = decodeWithParity . tail . channel . encodeWithParity

channel :: a -> a
channel = id

count :: (Eq a) => a -> [a] -> Int
count needle = length . filter (== needle)

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : filter (/= x) (removeDuplicates xs)

result :: (Ord a) => [a] -> [(Int, a)]
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

rank :: (Ord a) => [[a]] -> [a]
rank = map snd . result . map head

winner' :: (Ord a) => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [] -> undefined
  [c] -> c -- Only one left, this is the winner.
  (c : _) -> winner' (elim c bs) -- Ranked in increasing order, so this can be removed.

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f xs = [f x | x <- xs, p x]

filterMap' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap' p m = map m . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr ((:) . f) []

map'''' :: (a -> b) -> [a] -> [b]
map'''' f = unfold null (f . head) tail

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p = foldr ((++) . (\x -> [x | p x])) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : unfold (const False) f f x

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0

isIn :: (Eq a) => a -> [a] -> Bool
isIn needle = foldr ((||) . (== needle)) False

select :: (Ord a) => a -> a -> [a] -> [a]
select low high = filter (\x -> x <= high && x >= low)

capitals :: [Char] -> [Char]
capitals xs = [toUpper x | x <- xs, isAlpha x]

capitals' :: [Char] -> [Char]
capitals' = map toUpper . filter isAlpha

sumPositionsEqual :: (Eq a) => [a] -> [a] -> Int
sumPositionsEqual xs ys = sum [i | (x, y, i) <- zip3 xs ys [0 ..], x == y]

minList :: (Ord a) => [a] -> a
minList [] = undefined
minList (x : xs) = foldl min x xs

findPalindromes :: (Eq a) => [a] -> [[a]]
findPalindromes = filter isPalindrome . findSubstrings

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs && (length xs >= 2)

findSubstrings :: [a] -> [[a]]
findSubstrings = concat . map tails . inits

foldl1'' :: (a -> a -> a) -> [a] -> a
foldl1'' _ [] = undefined
foldl1'' _ [x] = x
foldl1'' f (x : xs) = f (foldl1'' f xs) x

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = undefined
foldr1' _ [x] = x
foldr1' f (x : xs) = foldr f x xs

-- foldl (+) 0 [1,2,3] = (((0+1)+2)+3)
foldl''' :: (b -> a -> b) -> b -> [a] -> b
foldl''' _ acc [] = acc
foldl''' f acc (x : xs) = f (foldl''' f acc xs) x

-- foldr (+) 0 [1,2,3] = (1+(2+(3+0)))
foldr''' :: (a -> b -> b) -> b -> [a] -> b
foldr''' _ acc [] = acc
foldr''' f acc (x : xs) = f x (foldr''' f acc xs)

minListl1 :: (Ord a) => [a] -> a
minListl1 = foldl1 min

minListr1 :: (Ord a) => [a] -> a
minListr1 = foldr1 min
