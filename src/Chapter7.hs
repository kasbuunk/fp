module Chapter7 where

add' :: Int -> Int -> Int
add' x y = x + y

twice :: (a -> a) -> a -> a
twice f a = f (f a)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
