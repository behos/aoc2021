module Main where

import           Debug.Trace
import           Lib

type Bounds = (Int, Int)

class Vel a where
  step :: (Int, a) -> (Int, a)
  done :: Bounds -> (Int, a) -> Bool

newtype XVel = XVel Int
instance Vel XVel where
  step (pos, XVel v) = (pos + v, XVel $ max 0 (v - 1))
  done (mn, mx) (pos, XVel v) = v == 0 && (pos < mn || pos > mx)

newtype YVel = YVel Int
instance Vel YVel where
  step (pos, YVel v) = (pos + v, YVel (v - 1))
  done (mn, _) (pos, _) = pos < mn

main :: IO ()
main = let
  (minx, maxx, miny, maxy) = (217, 240, -126, -69)
  in do
  putStrLn "Part 1"
  print $ maxHeight miny
  putStrLn "Part 2"
  print $ allVelocities minx maxx miny maxy

maxHeight :: Int -> Int
maxHeight y = div (abs y * (abs y - 1)) 2

allVelocities :: Int -> Int -> Int -> Int -> Int
allVelocities minx maxx miny maxy = let
  xRange = [0..maxx]
  allX = stepsInRange 0 (minx, maxx) . (0,) . XVel  <$> xRange
  yRange = [miny..(abs miny - 1)]
  allY = stepsInRange 0 (miny, maxy) . (0,) . YVel <$> yRange
  in matchingPairs allX allY

stepsInRange :: (Vel a) =>  Int -> Bounds -> (Int, a) -> [Int]
stepsInRange s bounds v | done bounds v = []
stepsInRange s bounds@(mn, mx) v@(pos, vel) =
  [s | mn <= pos && pos <= mx] ++ stepsInRange (s + 1) bounds (step v)

matchingPairs :: [[Int]] -> [[Int]] -> Int
matchingPairs [] _      = 0
matchingPairs (x:xs) ys = sum (fromEnum . matching x <$> ys) + matchingPairs xs ys

matching :: [Int] -> [Int] -> Bool
matching x y | null x || null y = False
matching ax@(x:xs) ay@(y:ys)
  | x == y = True
  | x < y = matching xs ay
  | otherwise = matching ax ys
