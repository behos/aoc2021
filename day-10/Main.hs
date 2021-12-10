module Main where

import           Data.List
import           Lib

data SyntaxError = Incomplete [Char] | Corrupted Char

main :: IO ()
main = do
  content <- readFile "inputs/day-10.txt"
  let errors = parse [] <$> lines content
  putStrLn "Part 1"
  print $ scoreCorrupted errors
  putStrLn "Part 2"
  print $ getCompletionScore errors

parse :: [Char] -> String -> SyntaxError
parse s []            = Incomplete s
parse (o:os) (c:p)
  | matching o == c = parse os p
parse s (c:p)
  | opening c = parse (c:s) p
  | otherwise = Corrupted c

scoreCorrupted :: [SyntaxError] -> Int
scoreCorrupted []               = 0
scoreCorrupted (Incomplete _:r) = scoreCorrupted r
scoreCorrupted (Corrupted c:r)  = points c + scoreCorrupted r

getCompletionScore :: [SyntaxError] -> Int
getCompletionScore err = let
  scores = scoreCompletions err
  in sort scores !! div (length scores) 2

scoreCompletions :: [SyntaxError] -> [Int]
scoreCompletions []               = []
scoreCompletions (Incomplete s:r) = scoreString (reverse s):scoreCompletions r
scoreCompletions (_:r)            = scoreCompletions r

scoreString :: [Char] -> Int
scoreString []    = 0
scoreString (c:r) = points c + 5 * scoreString r

opening :: Char -> Bool
opening c = c == '(' || c == '[' || c == '{' || c == '<'

matching :: Char -> Char
matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'

points :: Char -> Int
points '(' = 1
points '[' = 2
points '{' = 3
points '<' = 4
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
