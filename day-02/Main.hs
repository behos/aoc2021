module Main where

import           Lib

main :: IO ()
main = do
  content <- readFile "inputs/day-02.txt"
  let input = parseInput content
  putStrLn "Part 1"
  print $ result $ navigate input (Position (0, 0))
  putStrLn "Part 2"
  print $ result $ navigate input (AimPosition (0, 0, 0))

parseInput :: String -> [Direction]
parseInput content = map (pairer . words) $ lines content

pairer :: [String] -> Direction
pairer [n, v] = (n, Lib.toInt v)
pairer _      = error "invalid input"

type Direction = (String, Int)

newtype Position = Position (Int, Int)

newtype AimPosition = AimPosition (Int, Int, Int)

class Navigate a where
  move :: Direction -> a -> a
  result :: a -> Int

instance Navigate Position where
  move ("forward", n) (Position (p, d)) = Position (n + p, d)
  move ("down", n) (Position (p, d))    = Position (p, n + d)
  move ("up", n) (Position (p, d))      = Position (p, d - n)
  move _ _                              = error "invalid input"

  result (Position (p, d)) = p * d

instance Navigate AimPosition where
  move ("forward", n) (AimPosition (p, d, a)) =
    AimPosition (p + n, d + (n * a), a)
  move ("down", n) (AimPosition (p, d, a)) = AimPosition (p, d, a + n)
  move ("up", n) (AimPosition (p, d, a)) = AimPosition (p, d, a - n)
  move _ _ = error "invalid input"

  result (AimPosition (p, d, _)) = p * d

navigate :: (Navigate a) => [Direction] -> a -> a
navigate (dir : dirs) pos = navigate dirs $ move dir pos
navigate [] pos           = pos
