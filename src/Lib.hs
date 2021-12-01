module Lib
    ( toInt
    ) where

toInt :: String -> Int
toInt input = read input :: Int
