module Main where

import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as H
import           Data.List
import qualified Data.HashSet as S
import           Data.List.Split
import           Data.Maybe
import           Lib
import Debug.Trace

type Queue = (S.HashSet Point, [Point])
type CaveWeights = H.HashMap Point Int
type Caves = H.HashMap Point CaveWeights
type Distances = H.HashMap Point Int
type Points = S.HashSet Point

main :: IO ()
main = do
  content <- readFile "inputs/day-15.txt"
  let caveW = Lib.toHashMapIntMatrix content
  let distances = H.singleton (0, 0) 0
  let queue = (S.empty, [(0, 0)])
  putStrLn "Part 1"
  print $ shortestPaths (100, 100) caveW queue distances ! (99, 99)
  putStrLn "Part 2"
  print $ shortestPaths (500, 500) caveW queue distances ! (499, 499)

neighbors :: Point -> Point -> [Point]
neighbors (x, y) (w, h) = catMaybes [
  if x > 0 then Just (x - 1, y) else Nothing,
  if x < w - 1 then Just (x + 1, y) else Nothing,
  if y > 0 then Just (x, y - 1) else Nothing,
  if y < h - 1 then Just (x, y + 1) else Nothing
  ]

shortestPaths :: Point -> CaveWeights -> Queue -> Distances -> Distances
shortestPaths _ _ (_, []) d = d
shortestPaths dims cw queue d = let
  (p, popq) = pop queue
  (updq, updd) = foldr (
    \n (q, d) -> let
      dist = H.findWithDefault (maxBound :: Int) n d
      r = d ! p + risk cw n
      in if r < dist then
           let
             updd = H.insert n r d
             updq = push q n
           in (updq, updd)
         else (q, d)
    ) (popq, d) (neighbors p dims)
  in shortestPaths dims cw updq updd

risk :: CaveWeights -> Point -> Int
risk cw (x, y) = let
  tx = div x 100
  ty = div y 100
  dx = mod x 100
  dy = mod y 100
  val = cw ! (dx, dy) + tx + ty
  in mod val 10 + div val 10

push :: Queue -> Point -> Queue
push q@(s, l) p
  | S.member p s = q
  | otherwise = (S.insert p s, l ++ [p])

pop :: Queue -> (Point, Queue)
pop (s, p:rst) = (p, (S.delete p s, rst))
