module Lib
    ( toInt
    , toIntList
    ) where

toInt :: String -> Int
toInt input = read input :: Int

toIntList :: [String] -> [Int]
toIntList = map toInt
