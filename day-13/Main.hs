
module Main where

import           Lib
import           Data.List.Split
import           Data.List
import qualified Data.HashSet        as S

data Fold = X Int | Y Int
  deriving Show
type Points = S.HashSet Point

main :: IO ()
main = do
  content <- readFile "inputs/day-13.txt"
  let (points, folds@(first:_)) = parseInput content
  putStrLn "Part 1"
  print $ length $ foldPoints first points
  putStrLn "Part 2"
  putStrLn $ printPoints $ foldl' (flip foldPoints) points folds

parseInput :: String -> (Points, [Fold])
parseInput content = let
  [rawPoints, rawFolds] = splitOn "\n\n" content
  points = toPoint <$> lines rawPoints
  folds = toFold <$> lines rawFolds
  in (S.fromList points, folds)

toFold :: String -> Fold
toFold raw = let
  [_, _, instruction] = words raw
  in case splitOn "=" instruction of
    ["x", v] -> X $ toInt v
    ["y", v] -> Y $ toInt v

foldPoints :: Fold -> Points -> Points
foldPoints f p = let
  folded = S.map (foldPoint f) p
  in S.map (flip (op (-)) (getCorrection folded)) folded

foldPoint :: Fold -> Point -> Point
foldPoint (Y fy) (x, y) | y > fy= (x, 2 * fy - y)
foldPoint (X fx) (x, y) | x > fx = (2 * fx - x, y)
foldPoint  _ p = p

getCorrection :: Points -> Point
getCorrection = foldr (op min) (0, 0)

op :: (Int -> Int -> Int) -> Point -> Point -> Point
op f (x, y) (cx, cy) = (f x cx, f y cy)

printPoints :: Points -> String
printPoints p = let
  (mx, my) = foldr (op max) (0, 0) p
  in intercalate "\n" $ printRow p mx <$> [0..my]

printRow :: Points -> Int -> Int -> String
printRow p mx col = map
  (\row -> if S.member (row, col) p then 'O' else ' ') [0..mx]
