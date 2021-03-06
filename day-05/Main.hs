module Main where

import qualified Data.HashMap.Strict as HashMap
import           Data.List.Split
import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-05.txt"
  let lines = parseInput content
  putStrLn "Part 1"
  print $ findVents $ filter isStraight lines
  putStrLn "Part 2"
  print $ findVents lines

type Line = (Point, Point)

parseInput :: String -> [Line]
parseInput = (<$>) toLine . lines

toLine :: String -> Line
toLine raw =
  let (from:to:_) = splitOn " -> " raw
  in (toPoint from, toPoint to)

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

expand :: Line -> [Point]
expand line@(p1, p2)
  | p1 == p2 = [p1]
  | otherwise = p1 : expand (
    addPoints p1 $ direction line
    , p2
    )

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

findVents :: [Line] -> Int
findVents = length
  . filter (>= 2)
  . HashMap.elems
  . countPoints
  . concat
  . (expand <$>)

countPoints :: [Point] -> HashMap.HashMap Point Int
countPoints []     = HashMap.empty
countPoints (p:ps) = HashMap.insertWith (+) p 1 $ countPoints ps
