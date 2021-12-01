module Main where

import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-01.txt"
  putStrLn "Part 1"
  putStrLn $ show $ part1 content
  putStrLn "Part 2"
  putStrLn $ show $ part2 content


part1 :: String -> Int
part1 input = countIncreasing $ map Lib.toInt $ lines input

countIncreasing :: [Int] -> Int
countIncreasing (a:rest@(b:_))
  | a < b = 1 + countIncreasing rest
  | otherwise = countIncreasing rest
countIncreasing _ = 0

part2 :: String -> Int
part2 input = countWindows $ map Lib.toInt $ lines input

countWindows :: [Int] -> Int
countWindows (a:rest@(b:c:d:_))
  | a + b + c < b + c + d = 1 + countWindows rest
  | otherwise = countWindows rest
countWindows _ = 0
