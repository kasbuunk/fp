module Chapter11 where

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

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
