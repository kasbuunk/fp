module Chapter4 where

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: [a] -> Int -> ([a], [a])
splitAt' xs length_first = (take length_first xs, drop length_first xs)

reciprocal :: Fractional a => a -> a
reciprocal x = 1 / x

abs' :: (Num a, Ord a) => a -> a
abs' x = if x >= 0 then x else -x

signum' :: (Num a, Ord a) => a -> Int
signum' x = if x < 0 then -1 else if x == 0 then 0 else 1

abs'' :: (Num a, Ord a) => a -> a
abs'' x | x >= 0 = x
        | otherwise = -x

signum'' :: (Num a, Ord a) => a -> Int
signum'' x | x < 0 = -1
           | x == 0 = 0
           | x > 0 = 1

not' :: Bool -> Bool
not' False = True
not' True = False

and' :: Bool -> Bool -> Bool
and' True True = True
and' True False = False
and' False True = False
and' False False = False

and'' :: Bool -> Bool -> Bool
and'' True True = True
and'' _ _ = False

and''' :: Bool -> Bool -> Bool
and''' True b = b
and''' False _ = False

and'''' :: Bool -> Bool -> Bool
and'''' b c | b = c
            | otherwise = False

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, x) = x

startsWith :: Eq a => a -> [a] -> Bool
startsWith x (y:ys) = x == y

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

add' :: Num a => a -> a -> a
add' = \x -> (\y -> x + y)

const' :: a -> (b -> a)
const' x = (\_ -> x)

odds' :: Int -> [Int]
odds' n = map f [0..n-1] where
          f x = x*2 + 1

odds'' :: Int -> [Int]
odds'' n = map (\x -> x*2 + 1) [0..n-1]

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:(_:(x:_))) = x

safetail :: [a] -> [a]
safetail xs = if length xs == 0 then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | length xs == 0 = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

or' :: Bool -> Bool -> Bool
or' True _ = True
or' False a = a

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _ = True

or''' :: Bool -> Bool -> Bool
or''' True True = True
or''' False True = True
or''' True False = True
or''' False False = False

or'''' :: Bool -> Bool -> Bool
or'''' True _ = True
or'''' _ True = True
or'''' _ _ = False

luhnDouble :: Int -> Int
luhnDouble x | 2*x > 9 = 2*x - 9
             | 2*x <= 9 = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10) == 0
