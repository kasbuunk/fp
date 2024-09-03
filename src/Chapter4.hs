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
