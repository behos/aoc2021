module Main where

import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as H
import           Data.List
import           Lib

type Matrix = H.HashMap Point Int

main :: IO ()
main = do
  content <- readFile "inputs/day-11.txt"
  let matrix = Lib.toHashMapIntMatrix content
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
bumpPoint p m = let
  updated = H.adjust (+1) p m
  in case updated !? p of
    Just 10 -> foldr bumpPoint updated (neighbors p)
    _ -> updated

resetFlashing :: Matrix -> Matrix
resetFlashing = H.map (\v -> if v > 9 then 0 else v)
