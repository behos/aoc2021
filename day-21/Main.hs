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
  | odd round = let
      (p11, p21) = playUniverses factor (round + 1) (movePlayer p1 3) p2
      (p12, p22) = playUniverses (factor * 3) (round + 1) (movePlayer p1 4) p2
      (p13, p23) = playUniverses (factor * 6) (round + 1) (movePlayer p1 5) p2
      (p14, p24) = playUniverses (factor * 7) (round + 1) (movePlayer p1 6) p2
      (p15, p25) = playUniverses (factor * 6) (round + 1) (movePlayer p1 7) p2
      (p16, p26) = playUniverses (factor * 3) (round + 1) (movePlayer p1 8) p2
      (p17, p27) = playUniverses factor (round + 1) (movePlayer p1 9) p2
      in (p11 + p12 + p13 + p14 + p15 + p16 + p17, p21 + p22 + p23 + p24 + p25 + p26 + p27)
  | otherwise = let
      (p11, p21) = playUniverses factor (round + 1) p1 (movePlayer p2 3)
      (p12, p22) = playUniverses (factor * 3) (round + 1) p1 (movePlayer p2 4)
      (p13, p23) = playUniverses (factor * 6) (round + 1) p1 (movePlayer p2 5)
      (p14, p24) = playUniverses (factor * 7) (round + 1) p1 (movePlayer p2 6)
      (p15, p25) = playUniverses (factor * 6) (round + 1) p1 (movePlayer p2 7)
      (p16, p26) = playUniverses (factor * 3) (round + 1) p1 (movePlayer p2 8)
      (p17, p27) = playUniverses factor (round + 1) p1 (movePlayer p2 9)
    in (p11 + p12 + p13 + p14 + p15 + p16 + p17, p21 + p22 + p23 + p24 + p25 + p26 + p27)

probability :: Int -> Int
probability 3 = 1
probability 4 = 3
probability 5 = 6
probability 6 = 7
probability 7 = 6
probability 8 = 3
probability 9 = 1
