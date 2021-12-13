module Main where

import           Data.List
import           Lib

data SyntaxError = Incomplete [Char] | Corrupted Char

main :: IO ()
main = do
  content <- readFile "inputs/day-10.txt"
  let errors = parse [] <$> lines content
  putStrLn "Part 1"
  print $ sum $ scoreCorrupted <$> errors
  putStrLn "Part 2"
  print $ getCompletionScore errors

parse :: [Char] -> String -> SyntaxError
parse s [] = Incomplete s
parse (o:os) (c:p)
  | matching o == c = parse os p
parse s (c:p)
  | opening c = parse (c:s) p
  | otherwise = Corrupted c

scoreCorrupted :: SyntaxError -> Int
scoreCorrupted (Incomplete _) = 0
scoreCorrupted (Corrupted c)  = points c

getCompletionScore :: [SyntaxError] -> Int
getCompletionScore err = let
  scores = filter (>0) $ scoreCompletion <$> err
  in sort scores !! div (length scores) 2

scoreCompletion :: SyntaxError -> Int
scoreCompletion (Incomplete s) = foldl' (\p c -> points c + 5 * p) 0 s
scoreCompletion _              = 0

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
