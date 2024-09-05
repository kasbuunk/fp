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

surfaceRectangle :: Num a => a -> a -> a
surfaceRectangle height width = height*width

volumeCuboid :: Num a => a -> a -> a -> a
volumeCuboid height width depth = depth * surfaceRectangle height width

min2 :: (Ord a, Num a) => a -> a -> a
min2 x y | x <= y = x
         | otherwise = y

min2' :: (Ord a, Num a) => a -> a -> a
min2' x y = if x <= y then x else y

min3 :: (Ord a, Num a) => a -> a -> a -> a
min3 x y z = min2 (min2 x y) z

min3' :: (Ord a, Num a) => a -> a -> a -> a
min3' x y z = if x <= y then
                        if x <= z then
                                  x
                        else z
              else if y <= z
                   then y
                   else z

discriminant :: Float -> Float -> Float -> Float
discriminant a b c = b*b - 4 * a * c

abc :: (Float, Float, Float) -> [Float]
abc (a, b, c)
    | d < 0 = []
    | d == 0 = [singleRoot]
    | otherwise = [root1, root2]
    where
      d = discriminant a b c
      root1 = ((-1*b)+sqrt d) / (2*a)
      root2 = ((-1*b)-sqrt d) / (2*a)
      singleRoot = root1 -- root1 == root2

abc' :: Float -> Float -> Float -> [Float]
abc' a b c = let
                d = discriminant a b c
                root1 = ((-1*b)+sqrt d) / (2*a)
                root2 = ((-1*b)-sqrt d) / (2*a)
                singleRoot = root1 -- root1 == root2
             in
                if d < 0
                then []
                else
                    if d == 0
                    then [singleRoot]
                    else [root1, root2]

rootsQuadratic :: Float -> Float -> Float -> Int
rootsQuadratic a b c = length (abc' a b c)

rootsQuadratic' :: Float -> Float -> Float -> Int
rootsQuadratic' a b c
                    | d > 0 = 2
                    | d == 0 = 1
                    | otherwise = 0
                        where
                            d = discriminant a b c

isPositive :: Int -> Bool
isPositive n | n > 0 = True
             | otherwise = False

smallOrBig :: Int -> String
smallOrBig n | n < 5 = "smaller than 5"
             | n < 10 = "smaller than 10"
             | otherwise = "big"
