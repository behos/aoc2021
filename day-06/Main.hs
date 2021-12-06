module Main where

import           Data.List.Split
import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-06.txt"
  let fish = parseInput content
  putStrLn "Part 1"
  print $ generation 80 fish
  putStrLn "Part 2"
  print $ generation 256 fish

parseInput :: String -> [Int]
parseInput = (<$>) Lib.toInt . splitOn ","

generation :: Int -> [Int] -> Int
generation gen = sum . ((singleSchool . ((+ 7) . (gen -))) <$>)

singleSchool :: Int -> Int
singleSchool = (map school [0..] !!)
  where school n
          | n <= 7 = 1
          | n <= 9 = 2
          | otherwise = singleSchool (n - 7) + singleSchool (n - 9)
