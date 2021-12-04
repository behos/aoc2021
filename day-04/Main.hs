{-# LANGUAGE TupleSections #-}

module Main where

import Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.List.Split
import Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-04.txt"
  let (input, boards) = parseInput content
  putStrLn "Part 1"
  print $ play input boards
  putStrLn "Part 2"
  print $ playToLose input boards

type Point = (Int, Int)

data PointState = PointState
  { checked :: Bool,
    val :: Int
  }
  deriving (Show)

data Board = Board
  { numPoints :: HashMap.HashMap Int Point,
    pointStates :: HashMap.HashMap Point PointState
  }
  deriving (Show)

parseInput :: String -> ([Int], [Board])
parseInput content = do
  let (rawInputs : rawBoards) = splitOn "\n\n" content
  let inputs = Lib.toInt <$> splitOn "," rawInputs
  let boards = boardFromString <$> rawBoards
  (inputs, boards)

boardFromString :: String -> Board
boardFromString content =
  boardFromVec $
    asPointPairs 0 0 $
      Lib.toIntList . words <$> lines content

asPointPairs :: Int -> Int -> [[Int]] -> [(Point, Int)]
asPointPairs x y ((xy : xs) : ys) = ((x, y), xy) : asPointPairs (x + 1) y (xs : ys)
asPointPairs _ y ([] : ys) = asPointPairs 0 (y + 1) ys
asPointPairs _ _ [] = []

boardFromVec :: [(Point, Int)] -> Board
boardFromVec [] = Board {numPoints = HashMap.empty, pointStates = HashMap.empty}
boardFromVec ((point, x) : xs) = do
  let board = boardFromVec xs
  board
    { numPoints =
        HashMap.insert x point $
          numPoints board,
      pointStates =
        HashMap.insert point PointState {checked = False, val = x} $
          pointStates board
    }

play :: [Int] -> [Board] -> Int
play input boards = do
  let (num, board) = head $ winningBoards input boards
  calculateScore board num

playToLose :: [Int] -> [Board] -> Int
playToLose input boards = do
  let (num, board) = last $ winningBoards input boards
  calculateScore board num

winningBoards :: [Int] -> [Board] -> [(Int, Board)]
winningBoards [] _ = error "not all boards won"
winningBoards _ [] = []
winningBoards (x : xs) boards = do
  let (winners, losers) =
        partition (wonWithNumber x) $
          processNumber x <$> boards
  map (x,) winners ++ winningBoards xs losers

processNumber :: Int -> Board -> Board
processNumber x board = case numPoints board !? x of
  Nothing -> board
  Just point ->
    board
      { pointStates =
          HashMap.insert point PointState {checked = True, val = x} $
            pointStates board
      }

wonWithNumber :: Int -> Board -> Bool
wonWithNumber num board = case numPoints board !? num of
  Nothing -> False
  Just (x, y) -> isComplete board (row x) || isComplete board (col y)

isComplete :: Board -> [Point] -> Bool
isComplete = all . isChecked

isChecked :: Board -> Point -> Bool
isChecked board = checked . (pointStates board !)

row :: Int -> [Point]
row x = (x,) <$> [0 .. 4]

col :: Int -> [Point]
col y = (,y) <$> [0 .. 4]

calculateScore :: Board -> Int -> Int
calculateScore = (*) . sum . uncheckedVals

uncheckedVals :: Board -> [Int]
uncheckedVals = map val . filter (not . checked) . HashMap.elems . pointStates
