module Chapter16 where

power :: Int -> Int -> Int
power x n
  | n == 0 = 1
  | otherwise = x * power x (n - 1)

data Nat = Zero | Succ Nat deriving (Show)

sub :: Nat -> Nat -> Nat
sub Zero m = Zero
sub (Succ n) Zero = Succ n
sub (Succ n) (Succ m) = sub n m
