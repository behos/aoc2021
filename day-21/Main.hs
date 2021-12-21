module Main where

import           Data.List
import           Debug.Trace
import qualified Data.HashMap.Strict as H

type Die = [(Int, Int)]
type Rolls = [Int]
data Player = Player { pos :: Int, score :: Int } deriving Show
type Board = (Player, Player, Die)
type ULengths = H.HashMap Int Int

main :: IO ()
main = do
  let player1 = Player { pos = 9, score = 0 }
  let player2 = Player { pos = 8, score = 0 }
  let die = getDie 1 100
  putStrLn "Part 1"
  print $ playToWin (player1, player2, die)
  putStrLn "Part 2"
  print $ playUniverses 1 1 player1 player2

getDie :: Int -> Int -> Die
getDie roll sides = zip [1..] $ getRolls roll sides

getRolls :: Int -> Int -> Rolls
getRolls roll sides = roll : getRolls (if roll == sides then 1 else roll + 1) sides

playToWin :: Board -> Int
playToWin (p1, p2, (r, _):_)
  | score p2 >= 1000 = score p1 * (r - 1)
playToWin b = playToWin (playRound b)

playRound :: Board -> Board
playRound (p1, p2, (_, r1):(_, r2):(_, r3):d) =
  (p2, movePlayer p1 (r1 + r2 + r3), d)

movePlayer :: Player -> Int -> Player
movePlayer p roll = let
  pos' = mod (pos p + roll) 10
  in Player { pos = pos', score = score p + pos' + 1 }

playUniverses :: Integer -> Int -> Player -> Player -> (Integer, Integer)
playUniverses factor round p1 p2
  | score p1 >= 21 = (factor, 0)
  | score p2 >= 21 = (0, factor)
  | otherwise = let
      next = round + 1
      moveP1 = if odd round then movePlayer else const
      moveP2 = if even round then movePlayer else const
      in foldl'
         (\(s1, s2) (f, m) -> let
             (s1', s2') = playUniverses (factor * f) next (moveP1 p1 m) (moveP2 p2 m)
             in (s1 + s1', s2 + s2')
         ) (0, 0) universes

universes :: [(Integer, Int)]
universes = [(1, 3), (3, 4), (6, 5), (7, 6), (6, 7), (3, 8), (1, 9)]
