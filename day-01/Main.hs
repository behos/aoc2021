module Main where

import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-01.txt"
  let input = map Lib.toInt $ lines content
  putStrLn "Part 1"
  putStrLn $ show $ countIncreasing input
  putStrLn "Part 2"
  putStrLn $ show $ countWindows input

countIncreasing :: [Int] -> Int
countIncreasing (a:rest@(b:_))
  | a < b = 1 + countIncreasing rest
  | otherwise = countIncreasing rest
countIncreasing _ = 0

countWindows :: [Int] -> Int
countWindows (a:rest@(b:c:d:_))
  | a + b + c < b + c + d = 1 + countWindows rest
  | otherwise = countWindows rest
countWindows _ = 0
