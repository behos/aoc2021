module Lib
    ( toInt
    , toIntList
    , to2dVec
    , toHashMapMatrix
    , toHashMapIntMatrix
    , toPoint
    , Point
    , direction
    , intUnit
    , neighbors
    ) where

import qualified Data.HashMap.Strict as H
import           Data.List.Split
import qualified Data.Vector         as V

type Point = (Int, Int)

toPoint :: String -> Point
toPoint input = let
  [x, y] = toInt <$> splitOn "," input
  in (x, y)

toInt :: String -> Int
toInt input = read input :: Int

toIntList :: [String] -> [Int]
toIntList = map toInt

to2dVec :: String -> V.Vector (V.Vector Int)
to2dVec input = V.unfoldr iter (toVec <$> lines input)

toHashMapIntMatrix :: String -> H.HashMap Point Int
toHashMapIntMatrix = toHashMapMatrix (toInt . (: []))

toHashMapMatrix :: (Char -> a) -> String -> H.HashMap Point a
toHashMapMatrix f = H.fromList
  . concatMap
  (\(i, line) ->
     (\(j, c) -> ((i, j), f c)) <$> zip [0..] line
  ) . zip [0..] . lines

toVec :: String -> V.Vector Int
toVec input = V.unfoldr iter $ toInt . (: []) <$> input

iter :: [a] -> Maybe (a, [a])
iter []     = Nothing
iter (x:xs) = Just (x, xs)

direction :: (Point, Point) -> Point
direction ((x1, y1), (x2, y2)) = pointUnit (x2 - x1, y2 - y1)

pointUnit :: Point -> Point
pointUnit (x, y) = (intUnit x, intUnit y)

intUnit :: Int -> Int
intUnit 0 = 0
intUnit x = div x $ abs x

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + dx, y + dy) | dx <- adjacent, dy <- adjacent]

adjacent :: [Int]
adjacent = [-1, 0, 1]
