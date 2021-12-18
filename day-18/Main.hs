module Main where

import           Data.Char
import           Data.List
import           Lib

data Element = Value Int | Pair (Element, Element)
  deriving (Show, Eq)

main :: IO ()
main = do
  content <- readFile "inputs/day-18.txt"
  let numbers = parse content
  putStrLn "Part 1"
  print $ sumMagnitude numbers
  putStrLn "Part 2"
  print $ maxMagnitude numbers

parse :: String -> [Element]
parse input = fst . parseNumber [] <$> lines input

parseNumber :: [Element] -> String -> (Element, String)
parseNumber _ ('[':rst) = let
  (first, rst') = parseNumber [] rst
  in parseNumber [first] rst'
parseNumber [first, second] (']':rst) =
  (Pair (first, second), rst)
parseNumber [first] (',':rst) = let
  (second, rst') = parseNumber [] rst
  in parseNumber [first, second] rst'
parseNumber _ (n:rst)
  | isDigit n = (Value (toInt [n]), rst)

sumMagnitude :: [Element] -> Int
sumMagnitude nums = magnitude $ sumNums nums

magnitude :: Element -> Int
magnitude (Value v) = v
magnitude (Pair (l, r)) = 3 * magnitude l + 2 * magnitude r

sumNums :: [Element] -> Element
sumNums (e:rst) = foldl' addNums e rst

addNums :: Element -> Element -> Element
addNums e1 e2 = doUntilStable reduce $ Pair (e1, e2)

doUntilStable :: (Element -> Element) -> Element -> Element
doUntilStable f e = let
  e' = f e
  in if e == e' then e' else doUntilStable f e'

reduce :: Element -> Element
reduce e = let
  e' = doUntilStable (fst . explode 0) e
  in if e' == e then split e' else e'

explode :: Int -> Element -> (Element, (Int, Int))
explode 4 (Pair (Value v1, Value v2)) = (Value 0, (v1, v2))
explode depth (Pair (e, Value v)) = let
  (exploded, (l, r)) = explode (depth + 1) e
  in (Pair (exploded, Value (v + r)), (l, 0))
explode depth (Pair (Value v, e)) = let
  (exploded, (l, r)) = explode (depth + 1) e
  in (Pair (Value (v + l), exploded), (0, r))
explode _ v@(Value _) = (v, (0, 0))
explode depth (Pair (e1, e2)) = let
  (e1', (l1, r1)) = explode (depth + 1) e1
  (e2', (l2, r2)) = explode (depth + 1) e2
  in if e1' == e1
  then (Pair (addRight e1 l2, e2'), (0, r2))
  else (Pair (e1', addLeft e2 r1), (l1, 0))

addRight :: Element -> Int -> Element
addRight (Value v) d = Value (v + d)
addRight (Pair (l, r)) d = Pair (l, addRight r d)

addLeft :: Element -> Int -> Element
addLeft (Value v) d = Value (v + d)
addLeft (Pair (l, r)) d = Pair (addLeft l d, r)

split :: Element -> Element
split e@(Value v)
  | v < 10 = e
  | otherwise = Pair (Value (div v 2), Value (div v 2 + rem v 2))
split (Pair (e1, e2)) = let
  e1' = split e1
  e2' = split e2
  in if e1' == e1 then Pair(e1, e2') else Pair(e1', e2)

maxMagnitude :: [Element] -> Int
maxMagnitude els = maximum $ concat
  [ [sumMagnitude [e1, e2], sumMagnitude [e2, e1]]
  | (e1:els') <- tails els, e2 <- els']
