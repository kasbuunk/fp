module Chapter8 where

type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find key table = head [value | (x, value) <- table, key == x]
