module Main where

import           Data.List
import           Data.List.Split
import           Debug.Trace
import qualified Lib

type Point = (Int, Int, Int)
type Cuboid = (Point, Point)
data Op = On | Off deriving (Show, Eq)
type Cuboids = [Cuboid]
type CuboidOp = (Op, Cuboid)

main :: IO ()
main = do
  content <- readFile "inputs/day-22.txt"
  let cuboidOps = parse content
  let (firstBoot, secondBoot) = splitAt 20 cuboidOps
  putStrLn "Part 1"
  let litCuboids = foldl' doOps [] firstBoot
  print $ sum $ volume <$> litCuboids
  putStrLn "Part 2"
  let finalLitCuboids = foldl' doOps litCuboids secondBoot
  print $ sum $ volume <$> finalLitCuboids

parse :: String -> [CuboidOp]
parse s = parseCuboidOp <$> lines s

parseCuboidOp :: String -> CuboidOp
parseCuboidOp s = let
  [rawOp, rawCuboid] = words s
  in (parseOp rawOp, parseCuboid rawCuboid)

parseOp :: String -> Op
parseOp "on"  = On
parseOp "off" = Off

parseCuboid :: String -> Cuboid
parseCuboid s = let
  ranges = splitOn "," s
  [(mnx, mxx), (mny, mxy), (mnz, mxz)] = parseRange <$> ranges
  in ((mnx, mny, mnz), (mxx, mxy, mxz))

parseRange :: String -> (Int, Int)
parseRange s = let
  [_, rawRange] = splitOn "=" s
  [mn, mx] = Lib.toInt <$> splitOn ".." rawRange
  in (mn, mx)

doOps :: Cuboids -> CuboidOp -> Cuboids
doOps lit@(cl:rstlit) (op, cop)
  | cl == cop = if op == On then lit else rstlit
  | overlapping cl cop = let
      litSplit = splitCuboid cl cop
      opSplit = splitCuboid cop cl
    in foldl' doOps (litSplit ++ rstlit) ((op,) <$> opSplit)
  | otherwise = cl : doOps rstlit (op, cop)
doOps [] (Off, cop) = []
doOps [] (On, cop) = [cop]

splitOverlaps :: Cuboids -> Cuboid -> Cuboids
splitOverlaps [] _ = []
splitOverlaps
  (c@((mnx, mny, mnz), (mxx, mxy, mxz)):rst)
  nc@((mnx', mny', mnz'), (mxx', mxy', mxz'))
  | overlapping c nc = splitCuboid c nc ++ splitOverlaps rst nc
  | otherwise = c : splitOverlaps rst nc

overlapping :: Cuboid -> Cuboid -> Bool
overlapping ((mnx, mny, mnz), (mxx, mxy, mxz)) ((mnx', mny', mnz'), (mxx', mxy', mxz')) =
  not (
  mxx < mnx' || mxx' < mnx ||
  mxy < mny' || mxy' < mny ||
  mxz < mnz' || mxz' < mnz
  )

splitCuboid :: Cuboid -> Cuboid -> Cuboids
splitCuboid ((mnx, mny, mnz), (mxx, mxy, mxz)) ((mnx', mny', mnz'), (mxx', mxy', mxz')) = let
  (lx, rx) = intersectionPoints mnx mxx mnx' mxx'
  (clx, cmx, crx) = (
    ((mnx, mny, mnz), (lx - 1, mxy, mxz)),
    ((lx, mny, mnz), (rx, mxy, mxz)),
    ((rx + 1, mny, mnz), (mxx, mxy, mxz))
    )

  (ly, ry) = intersectionPoints mny mxy mny' mxy'
  (cly, cmy, cry) = (
    ((lx, mny, mnz), (rx, ly - 1, mxz)),
    ((lx, ly, mnz), (rx, ry, mxz)),
    ((lx, ry + 1, mnz), (rx, mxy, mxz))
    )

  (lz, rz) = intersectionPoints mnz mxz mnz' mxz'
  (clz, cmz, crz) = (
    ((lx, ly, mnz), (rx, ry, lz - 1)),
    ((lx, ly, lz), (rx, ry, rz)),
    ((lx, ly, rz + 1), (rx, ry, mxz))
    )
  in filter ((>0) . volume) [clz, cmz, crz, cly, cry, clx, crx]

intersectionPoints :: Int -> Int -> Int -> Int -> (Int, Int)
intersectionPoints mn mx mn' mx' = (max mn mn', min mx mx')

volume :: Cuboid -> Int
volume ((mnx, mny, mnz), (mxx, mxy, mxz)) =
  (mxx - mnx + 1) * (mxy - mny + 1) * (mxz - mnz + 1)
