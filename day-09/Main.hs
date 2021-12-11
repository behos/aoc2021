module Main where

import qualified Data.HashSet as S
import           Data.List
import           Data.Maybe
import           Data.Vector  ((!))
import qualified Data.Vector  as V
import           Lib

type Matrix = V.Vector (V.Vector Int)
type Group = S.HashSet Point

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
  $ (+ 1) . (\(x, y) -> m ! x ! y) <$> S.toList (minimalPoints m (0, 0))

minimalPoints :: Matrix -> Point -> Group
minimalPoints m p@(x, y)
  | x == V.length m = S.empty
  | y == V.length (m ! 0) = minimalPoints m (x + 1, 0)
  | otherwise = S.union
    (if isMinimal m p then S.singleton p else S.empty)
    $ minimalPoints m (x, y + 1)

isMinimal :: Matrix -> Point -> Bool
isMinimal m p@(x, y) = all ((m ! x ! y <) . (\(x, y) -> m ! x ! y)) $ getNeighbors m p

getNeighbors :: Matrix -> Point -> Group
getNeighbors m (x, y) =  S.fromList $ catMaybes [
  if x < V.length m - 1 then Just (x + 1, y) else Nothing,
  if x > 0 then Just (x - 1, y) else Nothing,
  if y < V.length (m ! 0) - 1 then Just (x, y + 1) else Nothing,
  if y > 0 then Just (x, y - 1) else Nothing
  ]

findLargestBasins :: Matrix -> Int
findLargestBasins m = let
  (a:b:c:_) = sortBy (flip compare)
    $ S.size <$> S.toList ( S.map (getBasin m . S.singleton) $ minimalPoints m (0, 0))
  in a * b * c

getBasin :: Matrix -> Group -> Group
getBasin m g = let
  neighbors = foldr S.union g $ S.map (getNeighbors m) g
  expanded = S.filter (\(x, y) -> m ! x ! y < 9) neighbors
  in
  if expanded == g
  then expanded
  else getBasin m expanded
