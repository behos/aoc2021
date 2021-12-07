module Main where

import           Data.List
import           Data.List.Split
import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-07.txt"
  let crabPositions = parseInput content
  putStrLn "Part 1"
  print $ goToMedian crabPositions
  putStrLn "Part 2"
  print $ goToAverage crabPositions

parseInput :: String -> [Int]
parseInput = (<$>) Lib.toInt . splitOn ","

goToMedian :: [Int] -> Int
goToMedian positions = let
  median = sort positions !! div (length positions) 2
  in sum $ abs . (median-) <$> positions

goToAverage :: [Int] -> Int
goToAverage positions = let
  average = div (sum positions) (length positions)
  in sum $ realFuelCost . abs . (average-) <$> positions

realFuelCost :: Int -> Int
realFuelCost distance = div ((1 + distance) * distance) 2
