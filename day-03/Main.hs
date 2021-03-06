module Main where

import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-03.txt"
  let input = parseInput content
  putStrLn "Part 1"
  print $ powerConsumption input
  putStrLn "Part 2"
  print $ lifeSupportRating input

parseInput :: String -> [[Int]]
parseInput content = map toIntSeq $ lines content

toIntSeq :: [Char] -> [Int]
toIntSeq = map (Lib.toInt . (: []))

powerConsumption :: [[Int]] -> Int
powerConsumption metrics =
  let gamma = calculateGamma metrics
      epsilon = invert gamma
  in toDec gamma * toDec epsilon

calculateGamma :: [[Int]] -> [Int]
calculateGamma (m : ms) =
  let (total, count) = foldr addDatapoint (m, 1) ms
      threshold = div count 2 + mod count 2 -- round up division in case of equality
  in normalize total threshold
calculateGamma [] = error "did not receive any metrics"

calculateEpsilon :: [[Int]] -> [Int]
calculateEpsilon = invert . calculateGamma

addDatapoint :: [Int] -> ([Int], Int) -> ([Int], Int)
addDatapoint datapoint (runningTotal, count) =
  (add runningTotal datapoint, count + 1)

add :: [Int] -> [Int] -> [Int]
add [] []             = []
add (a : as) (b : bs) = (a + b) : add as bs
add _ _               = error "invalid input"

normalize :: [Int] -> Int -> [Int]
normalize (a : as) threshold
  | a >= threshold = 1 : normalize as threshold
  | otherwise = 0 : normalize as threshold
normalize [] _ = []

invert :: [Int] -> [Int]
invert = map (1 -)

toDec :: [Int] -> Int
toDec = foldr (\x y -> x + 2 * y) 0 . reverse

maskFilter :: ([[Int]] -> [Int]) -> [[Int]] -> [Int]
maskFilter _ [] = error "invalid input"
maskFilter _ [result] = result -- if we only have one result, then we are done
maskFilter f metrics =
  let (m : _) = f metrics
      getMatchingSuffixes = map (\(_ : rs) -> rs) . filter (\(r : _) -> r == m)
  in m : maskFilter f (getMatchingSuffixes metrics)

lifeSupportRating :: [[Int]] -> Int
lifeSupportRating metrics =
  let oxygenGeneratorRating = maskFilter calculateGamma metrics
      co2ScrubberRating = maskFilter calculateEpsilon metrics
  in toDec oxygenGeneratorRating * toDec co2ScrubberRating
