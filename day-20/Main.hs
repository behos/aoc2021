module Main where

import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List.Split
import           Data.Vector         ((!))
import qualified Data.Vector         as V
import           Lib

type Lookup = V.Vector Char
type Image = H.HashMap Point Char

main :: IO ()
main = do
  content <- readFile "inputs/day-20.txt"
  let (lookup, image) = parse content
  putStrLn "Part 1"
  print $ countLit $ enhanceTimes 2 lookup image
  putStrLn "Part 2"
  print $ countLit $ enhanceTimes 50 lookup image

parse :: String -> (Lookup, Image)
parse content = let
  [rawLookup, rawImage] = splitOn "\n\n" content
  in (V.fromList rawLookup, toHashMapMatrix id rawImage)

countLit :: Image -> Int
countLit = H.size . H.filter (== '#')

enhanceTimes :: Int -> Lookup -> Image -> Image
enhanceTimes 0 _ i = i
enhanceTimes n l i = enhanceTimes (n - 1) l
  $ enhance l i (if even n then '.' else '#')

enhance :: Lookup -> Image -> Char -> Image
enhance l i d = let
  topLeft = foldl' (dims min) (0, 0) $ H.keys i
  bottomRight = foldl' (dims max) (0, 0) $ H.keys i
  topLeft' = dims (+) (-1, -1) topLeft
  bottomRight' = dims (+) (1, 1) bottomRight
  in setPoints i l d topLeft' topLeft' bottomRight' H.empty

dims :: (Int -> Int -> Int) -> Point -> Point -> Point
dims f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

setPoints :: Image -> Lookup -> Char -> Point -> Point -> Point -> Image -> Image
setPoints i l d (x, y) tl@(tx, ty) br@(bx, by) ei
  | y > by = ei
  | x > bx = setPoints i l d (tx, y + 1) tl br ei
setPoints i l d p@(x, y) tl br ei = let
  ns = flip (H.findWithDefault d) i <$> neighbors p
  idx = toDec ns
  in setPoints i l d (x + 1, y) tl br $ H.insert p (l ! idx) ei

toDec :: String -> Int
toDec = foldl' (\acc c -> acc * 2 + (if c == '#' then 1 else 0)) 0

printImage :: Image -> String
printImage i = let
  (mnx, mny) = foldl' (dims min) (0, 0) $ H.keys i
  (mxx, mxy) = foldl' (dims max) (0, 0) $ H.keys i
  in intercalate "\n" $ printRow i mnx mxx <$> [mny..mxy]

printRow :: Image -> Int -> Int -> Int -> String
printRow i mn mx col = map
  (\row -> (H.!) i (col, row)) [mn..mx]
