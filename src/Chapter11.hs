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
diag g = [x !! n | (x, n) <- zip g [0 .. size - 1]]
