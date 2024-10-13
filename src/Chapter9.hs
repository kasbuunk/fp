module Chapter9 where

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

-- Optimised
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = y /= 0 && x `mod` y == 0 && y /= 1
valid' Exp x y = x > 1 && y > 1

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp x y = x > 1 && y > 1

valid'' :: Op -> Int -> Int -> Bool
valid'' Add _ _ = True
valid'' Sub _ _ = True
valid'' Mul _ _ = True
valid'' Div x y = y /= 0 && x `mod` y == 0
valid'' Exp x y = x > 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o e e') = "(" ++ show e ++ show o ++ show e' ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

eval' :: Expr -> [Int]
eval' (Val n) = [n]
eval' (App o l r) = [apply o x y | x <- eval' l, y <- eval' r, valid'' o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) ys
  | x `elem` ys = isChoice xs (rmFirstOccurence x ys)
  | otherwise = False

rmFirstOccurence :: (Eq a) => a -> [a] -> [a]
rmFirstOccurence x [] = []
rmFirstOccurence x (y : ys)
  | x == y = ys
  | otherwise = y : rmFirstOccurence x ys

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

opsWithExp :: [Op]
opsWithExp = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry
  ]

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns =
  [ res | (ls, rs) <- split ns, lx <- results' ls, ry <- results' rs, res <- combine'' lx ry
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

combine'' :: Result -> Result -> [Result]
combine'' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, n') <- results ns', n == n']

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e, n') <- results' ns', n == n']
