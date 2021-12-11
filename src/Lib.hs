module Lib
    ( toInt
    , toIntList
    , to2dVec
    , toHashMapMatrix
    , Point
    ) where

import qualified Data.HashMap.Strict as H
import qualified Data.Vector         as V

type Point = (Int, Int)

toInt :: String -> Int
toInt input = read input :: Int

toIntList :: [String] -> [Int]
toIntList = map toInt

to2dVec :: String -> V.Vector (V.Vector Int)
to2dVec input = V.unfoldr iter (toVec <$> lines input)

toHashMapMatrix :: String -> H.HashMap Point Int
toHashMapMatrix = H.fromList
  . concatMap
  (\(i, line) ->
     (\(j, c) -> ((i, j), toInt [c])) <$> zip [0..] line
  ) . zip [0..] . lines


toVec :: String -> V.Vector Int
toVec input = V.unfoldr iter $ toInt . (: []) <$> input

iter :: [a] -> Maybe (a, [a])
iter []     = Nothing
iter (x:xs) = Just (x, xs)
