module Chapter8 where

import Chapter7 (removeDuplicates)

type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find key table = head [value | (x, value) <- table, key == x]

findAll :: (Eq k) => k -> Assoc k v -> [v]
findAll key table = [value | (x, value) <- table, key == x]

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

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x : _) = Just x

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop | Or Prop Prop | Iff Prop Prop

type Subst = Assoc Char Bool

tautology :: Prop -> Bool
tautology (Const b) = b
tautology p = and [evaluate s p | s <- substitutes p]

evaluate :: Subst -> Prop -> Bool
evaluate _ (Const b) = b
evaluate s (Var x) = find x s
evaluate s (Not p) = not (evaluate s p)
evaluate s (And p q) = evaluate s p && evaluate s q
evaluate s (Or p q) = evaluate s p || evaluate s q
evaluate s (Imply p q) = evaluate s p <= evaluate s q
evaluate s (Iff p q) = evaluate s p == evaluate s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Iff p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = []
bools 1 = [[False], [True]]
bools x = [False : x' | x' <- bools (x - 1)] ++ [True : x' | x' <- bools (x - 1)]

substitutes :: Prop -> [Subst]
substitutes p = map (zip vs) (bools (length vs))
  where
    vs = removeDuplicates (vars p)

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

type Cont = [Op]

data Op = EVAL_ADD Expr | EVAL_MULT Expr | ADD Int | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL_ADD y : c) n = eval y (ADD n : c)
exec (EVAL_MULT y : c) n = eval y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval e []

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ x) y = add y (mult x y)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x - 1))

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: (Ord a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node n y m)
  | x == y = True
  | otherwise = occurs x n || occurs x m

occurs' :: (Ord a) => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node n y m) = case compare x y of
  LT -> occurs' x n
  GT -> occurs' x m
  EQ -> True

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' x y) = abs (numLeaves x - numLeaves y) <= 1

numLeaves :: Tree' a -> Int
numLeaves (Leaf' _) = 1
numLeaves (Node' x y) = numLeaves x + numLeaves y

balance :: [a] -> Tree' a
balance [] = undefined
balance [x] = Leaf' x
balance xs = Node' (balance firstHalf) (balance secondHalf)
  where
    (firstHalf, secondHalf) = splitAtHalf xs

splitAtHalf :: [a] -> ([a], [a])
splitAtHalf xs = splitAt l xs
  where
    l = length xs `div` 2

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval' :: Expr -> Int
eval' = folde id (+)

testNumber :: Int -> Int -> Maybe Int
testNumber x y
  | x >= y && y >= 0 = Just y
  | otherwise = Nothing

data List a = Nil | Cons a (List a) deriving (Show)

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  Nil == _ = False
  _ == Nil = False
  Cons x l == Cons y l' = x == y && l == l'

datalist2list :: [a] -> List a
datalist2list = foldr Cons Nil

list2datalist :: List a -> [a]
list2datalist Nil = []
list2datalist (Cons x l) = x : list2datalist l
