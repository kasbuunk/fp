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
