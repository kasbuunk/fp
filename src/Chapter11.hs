module Chapter11 where

import Chapter10 (cls, goto)
import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

depth :: Int
depth = 9

type Grid = [[Player]]

data Player = O | B | X
  deriving (Show, Eq, Ord)

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    xs = length (filter (== X) ps)
    os = length (filter (== O) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = winningStrike `elem` strikes
  where
    winningStrike
      | p == X = replicate size X
      | p == O = replicate size O
      | otherwise = undefined
    strikes = rows ++ columns ++ diagonals
    rows = g
    columns = transpose g
    diagonals = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

showGrid :: Grid -> String
showGrid pps = unlines [showRow ps | ps <- pps]

showRow :: [Player] -> String
showRow [] = ""
showRow [p]
  | p == O = "O"
  | p == X = "X"
  | otherwise = " "
showRow (p : ps) = showRow [p] ++ "|" ++ showRow ps

putGrid :: Grid -> IO ()
putGrid = putStr . showGrid

valid :: Grid -> Int -> Bool
valid g n = n >= 0 && n < size ^ 2 && concat g !! n == B

move :: Grid -> Int -> Player -> [Grid]
move g n p =
  if valid g n
    then
      [ chop size (xs ++ [p] ++ ys)
      ]
    else []
  where
    (xs, B : ys) = splitAt n (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then
      return (read xs)
    else do
      putStrLn "Error: please type a number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player O wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise =
      do
        i <- getNat (prompt p)
        case move g i p of
          [] -> do
            putStrLn "Error: Invalid move"
            run' g p
          [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
  deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "Error: invalid move"
          play' g p
        [g'] -> play g' (next p)
  | p == X = do
      putStr "Player X is thinking... "
      (play $! bestmove g p) (next p)
