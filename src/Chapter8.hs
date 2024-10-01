module Chapter8 where

import Chapter7 (removeDuplicates)

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

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x : _) = Just x

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

type Subst = Assoc Char Bool

tautology :: Prop -> Bool
tautology (Const b) = b
tautology p = and [evaluate s p | s <- substitutes p]

evaluate :: Subst -> Prop -> Bool
evaluate _ (Const b) = b
evaluate s (Var x) = find x s
evaluate s (Not p) = not (evaluate s p)
evaluate s (And p q) = evaluate s p && evaluate s q
evaluate s (Imply p q) = evaluate s p <= evaluate s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = []
bools 1 = [[False], [True]]
bools x = [False : x' | x' <- bools (x - 1)] ++ [True : x' | x' <- bools (x - 1)]

substitutes :: Prop -> [Subst]
substitutes p = map (zip vs) (bools (length vs))
  where
    vs = removeDuplicates (vars p)
