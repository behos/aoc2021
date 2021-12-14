module Main where

import           Data.HashMap.Strict ((!?), (!))
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List.Split
import           Lib

type Rules = H.HashMap String Char
type Stock = H.HashMap String Int
type Counts = H.HashMap Char Int

main :: IO ()
main = do
  content <- readFile "inputs/day-14.txt"
  let (template, rules) = parseInput content
  putStrLn "Part 1"
  print $ stepDiff template rules 10
  putStrLn "Part 2"
  print $ stepDiff template rules 40

parseInput :: String -> (Stock, Rules)
parseInput content = let
  [rawStock, rawRules] = splitOn "\n\n" content
  in (compress rawStock, H.fromList $ toPair <$> lines rawRules)

toPair :: String -> (String, Char)
toPair raw = let [s, [c]] = splitOn " -> " raw in (s, c)

compress :: String -> Stock
compress [x]           = H.singleton [x, '$'] 1
compress (a:rst@(b:_)) = H.insertWith (+) [a, b] 1 (compress rst)

repeatPolymerize :: Int -> Rules -> Stock -> Stock
repeatPolymerize 0 r s = s
repeatPolymerize n r s = repeatPolymerize (n - 1) r (polymerize r s)

polymerize :: Rules -> Stock -> Stock
polymerize r s = foldr (produce r) H.empty $ H.toList s

produce :: Rules -> (String, Int) -> Stock -> Stock
produce r (p, s) acc = foldr (flip (H.insertWith (+)) s) acc $ expand r p

expand :: Rules -> String -> [String]
expand r p@[a, b] = case r !? p of
  Just v -> [[a, v], [v, b]]
  Nothing -> [p]

stepDiff :: Stock -> Rules -> Int -> Int
stepDiff t r s = countDiff $ repeatPolymerize s r t

countDiff :: Stock -> Int
countDiff t = let counts = H.elems $ count $ H.toList t in maximum counts - minimum counts

count :: [(String, Int)] -> Counts
count []     = H.empty
count (([s, _], c):xs) = H.insertWith (+) s c (count xs)
