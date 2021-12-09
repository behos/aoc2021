module Main where

import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet
import           Data.List
import           Data.List.Split
import           Lib
import           Text.Printf

type NumberSegments = HashSet.HashSet Char
type Entry = ([NumberSegments], [NumberSegments])
type NumberPairs = [(Int, NumberSegments)]
type NumberMapping = HashMap.HashMap Int NumberSegments
type NumberMatcher = NumberMapping -> NumberSegments -> Bool

main :: IO ()
main = do
  content <- readFile "inputs/day-08.txt"
  let entries = parseInput content
  putStrLn "Part 1"
  print $ sumEasyNums entries
  putStrLn "Part 2"
  print $ sumAll entries

sumEasyNums :: [Entry] -> Int
sumEasyNums = sum . (<$>) countEasyNums

countEasyNums :: Entry -> Int
countEasyNums (outputs, inputs) = let
  pairs = segmentMapping inputs
  numbers = toNumber pairs <$> outputs
  in length $ filter (\x -> x == 1 || x == 4 || x == 7 || x == 8) numbers

toNumber :: NumberPairs -> NumberSegments -> Int
toNumber [] _ = error "could not find number"
toNumber ((num, segments):rst) x
  | x == segments = num
  | otherwise = toNumber rst x

sumAll :: [Entry] -> Int
sumAll = sum . (<$>) fourDigitNum

fourDigitNum :: Entry -> Int
fourDigitNum (outputs, inputs) = let
  pairs = segmentMapping inputs
  [a, b, c, d] = toNumber pairs <$> outputs
  in a * 1000 + b * 100 + c * 10 + d

parseInput :: String -> [Entry]
parseInput = (<$>) toEntry . lines

toEntry :: String -> Entry
toEntry line = let
  (inputs:outputs:_) = splitOn " | " line
  in (asSegments outputs, asSegments inputs)

asSegments :: String -> [NumberSegments]
asSegments = (<$>) HashSet.fromList . words

segmentMapping :: [NumberSegments] -> NumberPairs
segmentMapping entries = HashMap.toList $ findNums HashMap.empty [1, 7, 4, 8, 6, 0, 9, 2, 5, 3] entries

findNums :: NumberMapping -> [Int] -> [NumberSegments] -> NumberMapping
findNums mapping [] _ = mapping
findNums mapping (x:xs) candidates = let
  (num, remaining) = findNum mapping x candidates
  in findNums (HashMap.insert x num mapping) xs remaining

findNum :: NumberMapping -> Int -> [NumberSegments] -> (NumberSegments, [NumberSegments])
findNum _ num [] = error $ printf "could not find number %d" num
findNum nums num (x:xs)
  | is num nums x = (x, xs)
  | otherwise = let
      (found, remaining) = findNum nums num xs
      in (found, x:remaining)

is :: Int -> NumberMatcher
is 0 = \numbers x -> let
  four = numbers ! 4
  eight = numbers ! 8
  in HashSet.size x == 6 && HashSet.size (HashSet.intersection four $ HashSet.difference eight x) == 1
is 1 = \numbers x -> HashSet.size x == 2
is 2 = \numbers x -> let
  four = numbers ! 4
  eight = numbers ! 8
  in HashSet.size x == 5 && HashSet.size (HashSet.intersection four $ HashSet.difference eight x) == 2
is 3 = \numbers x -> let
  four = numbers ! 4
  nine = numbers ! 9
  in HashSet.size x == 5 && HashSet.size (HashSet.intersection four $ HashSet.difference nine x) == 1
is 4 = \numbers x -> HashSet.size x == 4
is 5 = \numbers x -> let
  one = numbers ! 1
  nine = numbers ! 9
  in HashSet.size x == 5 && HashSet.size (HashSet.intersection one $ HashSet.difference nine x) == 1
is 6 = \numbers x -> let
  one = numbers ! 1
  eight = numbers ! 8
  in HashSet.size x == 6 && HashSet.size (HashSet.intersection one $ HashSet.difference eight x) == 1
is 7 = \numbers x -> HashSet.size x == 3
is 8 = \numbers x -> HashSet.size x == 7
is 9 = \numbers x -> let
  four = numbers ! 4
  eight = numbers ! 8
  in HashSet.size x == 6 && HashSet.size (HashSet.intersection four $ HashSet.difference eight x) == 0
