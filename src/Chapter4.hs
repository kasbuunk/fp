module Chapter4 where

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: [a] -> Int -> ([a], [a])
splitAt' xs length_first = (take length_first xs, drop length_first xs)

reciprocal :: Fractional a => a -> a
reciprocal x = 1 / x
