module Main where

import           Data.Char
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Lib

type Point = (Int, Int, Int)
type Points = S.HashSet Point

main :: IO ()
main = do
  content <- readFile "inputs/day-19.txt"
  let (beacons, scanners) = reduceUntilMerged $ (, [(0, 0, 0)]) <$> parse content
  putStrLn "Part 1"
  print $ length beacons
  putStrLn "Part 2"
  let distances = [ distance s1 s2 | s1 <- scanners, s2 <- scanners]
  print $ maximum distances

parse :: String -> [Points]
parse content = let
  scanners = splitOn "\n\n" content
  in toPoints <$> scanners

toPoints :: String -> Points
toPoints scanner = let
  (_:rawPoints) = lines scanner
  in S.fromList $ toPoint <$> rawPoints

toPoint :: String -> Point
toPoint input = let
  [x, y, z] = Lib.toInt <$> splitOn "," input
  in (x, y, z)

withOffset :: (Int -> Int -> Int) -> Point -> Point -> Point
withOffset f (dx, dy, dz) (x, y, z) = (f x dx, f y dy, f z dz)

distance :: Point -> Point -> Int
distance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

orientations :: [Point -> Point]
orientations = [ f . r | f <- flips, r <- rotations]

flips :: [Point -> Point]
flips =
  [ \(x, y, z) -> (x, y, z)
  , \(x, y, z) -> (x, -y, -z)
  , \(x, y, z) -> (x, z, -y)
  , \(x, y, z) -> (x, -z, y)
  ]

rotations :: [Point -> Point]
rotations =
  [ \(x, y, z) -> (x, y, z)
  , \(x, y, z) -> (y, -x, z)
  , \(x, y, z) -> (z, y, -x)
  , \(x, y, z) -> (-z, y, x)
  , \(x, y, z) -> (-y, x, z)
  , \(x, y, z) -> (-x, y, -z)
  ]

mergeOverlapping :: (Points, [Point]) -> (Points, [Point]) -> Maybe (Points, [Point])
mergeOverlapping (s1, os1) (s2, os2) = let
  oriented = [ (S.map f s2, f <$> os2) | f <- orientations]
  merged =
    [ let
        s1' = S.map (withOffset (-) o1) s1
        s2'' = S.map (withOffset (-) o2) s2'
      in (S.union s1' s2'', (withOffset (-) o1 <$> os1) ++ (withOffset (-) o2 <$> os2'))
    | o1  <- S.toList s1
    , (s2', os2') <- oriented
    , o2  <- S.toList s2'
    ]
  in case filter (\(m, _) -> length m + 12 <= (length s1 + length s2)) merged of
       (s:_) -> Just s
       []    -> Nothing

mergeScanner :: [(Points, [Point])] -> (Points, [Point]) -> [(Points, [Point])]
mergeScanner [] r = [r]
mergeScanner (r1:rst) r2 = case mergeOverlapping r1 r2 of
  Just m  -> m:rst
  Nothing -> r1 : mergeScanner rst r2

reduceUntilMerged :: [(Points, [Point])] -> (Points, [Point])
reduceUntilMerged rs = case foldl' mergeScanner [] rs of
  [r] -> r
  rs' -> reduceUntilMerged rs'
