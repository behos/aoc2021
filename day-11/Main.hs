module Main where

import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.Maybe
import           Lib

type Point = (Int, Int)
type Matrix = H.HashMap Point Int

main :: IO ()
main = do
  content <- readFile "inputs/day-11.txt"
  let matrix = Lib.toHashMapMatrix content
  putStrLn "Part 1"
  print $ totalFlashes 100 matrix
  putStrLn "Part 2"
  print $ stepUntilSynced matrix

totalFlashes :: Int -> Matrix -> Int
totalFlashes 0 _ = 0
totalFlashes steps matrix = let
  updated = doStep matrix
  in countFlashed updated + totalFlashes (steps - 1) updated

countFlashed :: Matrix -> Int
countFlashed = length . H.filter (== 0)

stepUntilSynced :: Matrix -> Int
stepUntilSynced m
  | countFlashed m == H.size m = 0
  | otherwise = 1 + stepUntilSynced (doStep m)

doStep :: Matrix -> Matrix
doStep = resetFlashing . bumpAll

bumpAll :: Matrix -> Matrix
bumpAll m = foldr bumpPoint m $ H.keys m

bumpPoint :: Point -> Matrix -> Matrix
bumpPoint p@(x, y) m
  | isNothing (m !? p) = m
  | m ! p == 9 = foldr bumpPoint (H.insert p 10 m) (neighbors p)
  | otherwise = H.adjust (+1) p m


resetFlashing :: Matrix -> Matrix
resetFlashing = H.map (\v -> if v > 9 then 0 else v)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + dx, y + dy) | dx <- adjacent, dy <- adjacent]

adjacent :: [Int]
adjacent = [-1, 0, 1]