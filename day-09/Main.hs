module Main where

import qualified Data.HashSet as S
import           Data.List
import           Data.Vector  ((!))
import qualified Data.Vector  as V
import           Lib

type Matrix = V.Vector (V.Vector Int)
type Group = S.HashSet (Int, Int)

main :: IO ()
main = do
  content <- readFile "inputs/day-09.txt"
  let matrix = Lib.to2dVec content
  putStrLn "Part 1"
  print $ sumRiskLevels matrix
  putStrLn "Part 2"
  print $ findLargestBasins matrix

sumRiskLevels :: Matrix -> Int
sumRiskLevels m = sum
  $ (+ 1) . (\(x, y) -> m ! x ! y) <$> S.toList (minimalPoints 0 0 m)

minimalPoints :: Int -> Int -> Matrix -> Group
minimalPoints x y m
  | x == V.length m = S.empty
  | y == V.length (m ! 0) = minimalPoints (x + 1) 0 m
  | otherwise =
    if isMinimal x y m
    then  S.insert (x, y) $ minimalPoints x (y + 1) m
    else minimalPoints x (y + 1) m

isMinimal :: Int -> Int -> Matrix -> Bool
isMinimal x y m = let
  in all ((m ! x ! y <) . (\(x, y) -> m ! x ! y)) $ getNeighbors x y m

getNeighbors :: Int -> Int -> Matrix -> Group
getNeighbors x y m =  S.fromList (
  getPoint (x + 1) y m ++
  getPoint (x - 1) y m ++
  getPoint x (y - 1) m ++
  getPoint x (y + 1) m
  )

getPoint :: Int -> Int -> Matrix -> [(Int, Int)]
getPoint x y m
  | x < 0 || y < 0 || x == V.length m || y == V.length (m ! 0) = []
  | otherwise = [(x, y)]

findLargestBasins :: Matrix -> Int
findLargestBasins m = let
  (a:b:c:_) = sortBy (flip compare)
    $ S.size <$> S.toList ( S.map (getBasin m . S.singleton) $ minimalPoints 0 0 m)
  in a * b * c

getBasin :: Matrix -> Group -> Group
getBasin m g = let
  neighbors = foldr S.union g (S.map (\(x, y) -> getNeighbors x y m) g)
  expanded = S.filter (\(x, y) -> m ! x ! y < 9) neighbors
  in
  if expanded == g
  then expanded
  else getBasin m expanded
