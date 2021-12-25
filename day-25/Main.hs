module Main where

import           Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as H
import           Data.List
import           Lib

data SeaCucumber = East | South
  deriving (Eq, Show)
data Seafloor = Seafloor
  { cucumbers :: H.HashMap Point SeaCucumber
  , dims :: Point
  } deriving (Show, Eq)

main :: IO ()
main = do
  seafloor <- parseSeafloor
  putStrLn "Part 1"
  print $ stepsUntilStillness seafloor

parseSeafloor :: IO Seafloor
parseSeafloor = do
  content <- readFile "inputs/day-25.txt"
  let dims = (length $ head (lines content), length $ lines content)
  return
    $ parsePoints (0, 0) (Seafloor { cucumbers = H.empty, dims = dims })
    $ lines content

parsePoints :: Point -> Seafloor -> [String] -> Seafloor
parsePoints _ s [] = s
parsePoints (x, y) s ([]:ss) = parsePoints (0, y + 1) s ss
parsePoints p@(x, y) s ((c:cs):ss) =
  parsePoints (x + 1, y) (insertCucumber p c s) (cs:ss)

insertCucumber :: Point -> Char -> Seafloor -> Seafloor
insertCucumber p 'v' s = s { cucumbers = H.insert p South (cucumbers s) }
insertCucumber p '>' s = s { cucumbers = H.insert p East (cucumbers s) }
insertCucumber _ _ s   = s

stepsUntilStillness :: Seafloor -> Int
stepsUntilStillness s = let
  s' = move East s
  s'' = move South s'
  in if s'' == s then 1 else 1 + stepsUntilStillness s''

move :: SeaCucumber -> Seafloor -> Seafloor
move c s = let
  pairs = H.toList $ H.filter (c==) $ cucumbers s
  in foldl' (moveCucumber s) (s { cucumbers = H.filter (c/=) $ cucumbers s }) pairs

moveCucumber :: Seafloor -> Seafloor -> (Point, SeaCucumber) -> Seafloor
moveCucumber s s' (p, c) = let
  dest = nextPoint c (dims s) p
  in s' { cucumbers = case cucumbers s !? dest of
            Nothing -> H.insert dest c (cucumbers s')
            Just _ -> H.insert p c (cucumbers s')
        }

nextPoint :: SeaCucumber -> Point -> Point -> Point
nextPoint East (mx, _) (x, y) | x + 1 == mx = (0, y)
nextPoint East _ (x, y) = (x + 1, y)
nextPoint South (_, my) (x, y) | y + 1 == my = (x, 0)
nextPoint South _ (x, y) = (x, y + 1)

printSeafloor :: Seafloor -> String
printSeafloor s = let
  (mx, my) = dims s
  in intercalate "\n" $ printRow s mx <$> [0..my - 1]

printRow :: Seafloor -> Int -> Int -> String
printRow s mx col = map
  (\row -> case cucumbers s !? (row, col) of
             Just South -> 'v'
             Just East -> '>'
             Nothing -> '.'
      ) [0..mx - 1]
