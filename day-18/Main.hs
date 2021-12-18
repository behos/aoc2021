module Main where

import           Data.Char
import           Data.List
import           Lib

data Element = Value Int | Pair (Element, Element)
  deriving (Show, Eq)

data Dir = L | R

main :: IO ()
main = do
  content <- readFile "inputs/day-18.txt"
  let numbers = parse content
  putStrLn "Part 1"
  print $ sumMagnitude numbers
  putStrLn "Part 2"
  print $ maxMagnitude numbers

parse :: String -> [Element]
parse input = fst . parseNumber <$> lines input

parseNumber :: String -> (Element, String)
parseNumber (n:rst) | isDigit n = (Value (toInt [n]), rst)
parseNumber ('[':rst) = let
  (l, ',':rst') = parseNumber rst
  (r, ']':rst'') = parseNumber rst'
  in (Pair (l, r), rst'')

sumMagnitude :: [Element] -> Int
sumMagnitude nums = magnitude $ sumNums nums

magnitude :: Element -> Int
magnitude (Value v)     = v
magnitude (Pair (l, r)) = 3 * magnitude l + 2 * magnitude r

sumNums :: [Element] -> Element
sumNums (e:rst) = foldl' addNums e rst

addNums :: Element -> Element -> Element
addNums e1 e2 = doUntilStable reduce $ Pair (e1, e2)

doUntilStable :: (Element -> Element) -> Element -> Element
doUntilStable f e = case f e of
  e' | e' == e -> e
     | otherwise -> doUntilStable f e'

reduce :: Element -> Element
reduce = split . doUntilStable (fst . explode 0)

explode :: Int -> Element -> (Element, (Int, Int))
explode 4 (Pair (Value v1, Value v2)) = (Value 0, (v1, v2))
explode _ v@(Value _) = (v, (0, 0))
explode depth (Pair (e1, e2)) = let
  (e1', (l1, r1)) = explode (depth + 1) e1
  (e2', (l2, r2)) = explode (depth + 1) e2
  in if e1' == e1
  then (Pair (add R e1 l2, e2'), (0, r2))
  else (Pair (e1', add L e2 r1), (l1, 0))

add :: Dir -> Element -> Int -> Element
add _ (Value v) d     = Value (v + d)
add R (Pair (l, r)) d = Pair (l, add R r d)
add L (Pair (l, r)) d = Pair (add L l d, r)

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
