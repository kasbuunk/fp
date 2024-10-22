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
