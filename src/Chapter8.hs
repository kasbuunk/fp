module Chapter8 where

type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find key table = head [value | (x, value) <- table, key == x]

data Move = North | South | East | West

type Pos = (Double, Double)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move West (x, y) = (x - 1, y)
move East (x, y) = (x + 1, y)

moves :: [Move] -> Pos -> Pos
moves [] pos = pos
moves (m : ms) pos = moves ms (move m pos)

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rectangle h w) = h * w
