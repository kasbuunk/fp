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
  won <- play secret
  print won

play :: String -> IO Bool
play secret = do
  guess <- getLine
  if guess == secret
    then
      return True
    else do
      putStrLn (match secret guess)
      play secret

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
