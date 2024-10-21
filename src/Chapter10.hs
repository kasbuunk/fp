module Chapter10 where

import Data.Char
import System.IO

firstAndThirdChar :: IO (Char, Char)
firstAndThirdChar = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then
      return []
    else do
      xs <- getLine'
      return (x : xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (c : cs) = do
  putChar c
  putStr' cs

putStrLn' :: String -> IO ()
putStrLn' cs = do
  putStr cs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr' "Enter a string: "
  cs <- getLine'
  putStr' "The line has "
  putStr' (show (length cs))
  putStrLn' " characters."

showDouble :: IO ()
showDouble = do
  c <- getChar
  putChar (double c)

showDouble' :: IO ()
showDouble' = do
  cs <- readNumber'
  putStrLn' (show (2 * cs))

double :: Char -> Char
double c
  | isDigit c && digitToInt c <= 4 = intToDigit (2 * digitToInt c)
  | otherwise = '?'

isNumber' :: String -> Bool
isNumber' [] = True
isNumber' (c : cs)
  | c >= '0' && c <= '9' = isNumber' cs
  | otherwise = False

readNumber' :: IO Int
readNumber' = do
  cs <- getLine
  if isNumber' cs then return (read cs) else readNumber'

askName :: IO (IO ())
askName = do
  putStr "What is your name? "
  name <- getLine
  return (putStrLn ("Hello, " ++ name ++ "."))

welcomeTwo :: IO ()
welcomeTwo = do
  person1 <- askName
  person2 <- askName
  person1
  person2

for' :: [a] -> (a -> IO ()) -> IO ()
for' [] _ = return ()
for' (n : ns) f = do
  f n
  for' ns f

hangman :: IO ()
hangman = do
  secret <- sgetLine
  won <- playHangman secret
  print won

playHangman :: String -> IO Bool
playHangman secret = do
  guess <- getLine
  if guess == secret
    then
      return True
    else do
      putStrLn (match secret guess)
      playHangman secret

match :: String -> String -> String
match xs ys = [if (elem x ys) then x else '-' | x <- xs]

sgetLine :: IO String
sgetLine =
  do
    x <- getCh
    if x == '\n'
      then do
        putChar x
        return []
      else do
        putChar '-'
        xs <- sgetLine
        return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [if n == (row - 1) then stars - num else stars | (stars, n) <- zip board [0 ..]]

putRow :: Int -> Int -> IO ()
putRow x y = do
  putStrLn (show x ++ ": " ++ concat (replicate y "* "))
  return ()

putBoard :: Board -> IO ()
putBoard [x, y, z, u, v] = do
  putRow 1 x
  putRow 2 y
  putRow 3 z
  putRow 4 u
  putRow 5 v

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then
      return (digitToInt x)
    else do
      putStrLn "Try a number."
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

playNim :: Board -> Int -> IO ()
playNim board player = do
  newline
  putBoard board
  if finished board
    then do
      newline
      putStr ("Player " ++ show (next player) ++ "wins!")
    else do
      newline
      putStr ("Player " ++ show player)
      newline
      row <- getDigit "Enter a row number: "
      num <- getDigit "Enter the number of stars to remove: "
      if valid board row num
        then
          playNim (move board row num) (next player)
        else do
          putStrLn "Error: invalid move"
          playNim board player

nim :: IO ()
nim = do
  playNim initial 1

life :: GameBoard -> IO ()
life b =
  do
    cls
    showcells b
    wait 500000
    life (nextgen b)

cls :: IO ()
cls = putStr "\ESC[H\ESC[2J"

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type GameBoard = [Pos]

glider :: GameBoard
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: GameBoard -> IO ()
showcells b = do
  sequence_ [writeat p "0" | p <- b]

isAlive :: GameBoard -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: GameBoard -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbours :: Pos -> [Pos]
neighbours (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) = (x `mod` width, y `mod` height)

liveneighbours :: GameBoard -> Pos -> Int
liveneighbours b = length . filter (isAlive b) . neighbours

survivors :: GameBoard -> [Pos]
survivors b = filter (\p -> liveneighbours b p `elem` [2, 3]) b

births :: GameBoard -> [Pos]
births b =
  [ (x, y)
    | x <- [0 .. width - 1],
      y <- [0 .. height - 1],
      isEmpty b (x, y),
      liveneighbours b (x, y) == 3
  ]

nextgen :: GameBoard -> GameBoard
nextgen b = survivors b ++ births b
