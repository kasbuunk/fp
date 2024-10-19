module Chapter10 where

import Data.Char

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

double :: Char -> Char
double c
  | isDigit c && digitToInt c <= 4 = intToDigit (2 * digitToInt c)
  | otherwise = '?'

isNumber' :: String -> Bool
isNumber' [] = True
isNumber' (c : cs)
  | c >= '0' && c <= '9' = isNumber' cs
  | otherwise = False
