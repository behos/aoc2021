module Main where

import           Debug.Trace
import           Lib

type StepFn = (Int -> Int -> Int -> Int -> [Int])

main :: IO ()
main = let
  (minx, maxx, miny, maxy) = (217, 240, -126, -69)
  in do
  putStrLn "Part 1"
  print $ maxHeight miny
  putStrLn "Part 2"
  print $ allVelocities minx maxx miny maxy

maxHeight :: Int -> Int
maxHeight y = div (abs y * (abs y - 1)) 2

allVelocities :: Int -> Int -> Int -> Int -> Int
allVelocities minx maxx miny maxy = let
  xRange = [0..maxx]
  allX = stepsInRange minx maxx xSteps <$> xRange
  yRange = [miny..(abs miny - 1)]
  allY = stepsInRange miny maxy ySteps <$> yRange
  in matchingPairs allX allY

stepsInRange :: Int -> Int -> StepFn -> Int -> [Int]
stepsInRange mn mx f v =
  fst <$> filter (\(i, p) -> p <= mx && p >= mn) (zip [0..] (f mn mx 0 v))

xSteps :: Int -> Int -> Int -> Int -> [Int]
-- avoid out of range infinity, our filters will never match
xSteps minx maxx pos 0 | pos < minx || pos > maxx = []
xSteps minx maxx pos vel = pos : xSteps minx maxx (pos + vel) (max 0 (vel - 1))

ySteps :: Int -> Int -> Int -> Int -> [Int]
ySteps miny _ pos _ | pos < miny = []
ySteps miny maxy pos vel = pos : ySteps miny maxy (pos + vel) (vel - 1)

matchingPairs :: [[Int]] -> [[Int]] -> Int
matchingPairs [] _      = 0
matchingPairs (x:xs) ys = length (filter (matching x) ys) + matchingPairs xs ys

matching :: [Int] -> [Int] -> Bool
matching _ [] = False
matching [] _ = False
matching (x:xs) (y:ys)
  | x == y = True
  | x < y = matching xs (y:ys)
  | otherwise = matching (x:xs) ys
