module Lib
    ( toInt
    , toIntList
    , to2dVec
    ) where

import qualified Data.Vector as V

toInt :: String -> Int
toInt input = read input :: Int

toIntList :: [String] -> [Int]
toIntList = map toInt

to2dVec :: String -> V.Vector (V.Vector Int)
to2dVec input = V.unfoldr iter (toVec <$> lines input)

toVec :: String -> V.Vector Int
toVec input = V.unfoldr iter $ toInt . (: []) <$> input

iter :: [a] -> Maybe (a, [a])
iter [] = Nothing
iter (x:xs) = Just (x, xs)
