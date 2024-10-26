module Chapter15 where

multByPos :: [Int] -> [Int]
multByPos xs = [x * p | (x, p) <- zip xs [1 ..]]

multByPos' :: [Int] -> [Int]
multByPos' = zipWith (*) [1 ..]

powers :: Int -> [Int]
powers n = iterate (* n) 1

firstPowers :: Int -> Int -> [Int]
firstPowers amount = take amount . powers

sumPowers :: Int -> Int -> Int
sumPowers amount = sum . firstPowers amount

next :: Float -> Float -> Float
next n x = (x + n / x) / 2

repeat' :: (Float -> Float -> Float) -> Float -> Float -> [Float]
repeat' f x y = y : repeat' f x (f x y)

repeat'' :: (Float -> Float) -> Float -> [Float]
repeat'' f x = iterate f x

closeEnough :: Float -> [Float] -> Float
closeEnough eps (x1 : x2 : xs)
  | abs (x1 - x2) <= eps = x2
  | otherwise = closeEnough eps (x2 : xs)

sqrt' :: Float -> Float -> Float -> Float
sqrt' x eps n = closeEnough eps (iterate (next n) x)
