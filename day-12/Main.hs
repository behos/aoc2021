{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Char
import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Hashable
import           Data.List.Split
import           Debug.Trace
import           GHC.Generics        (Generic)

data Cave = Small String | Big String | Start | End
  deriving (Eq, Generic, Show)
instance Hashable Cave

type CaveSet = S.HashSet Cave
type CaveSystem = H.HashMap Cave CaveSet
type Exclusion = (Bool, CaveSet)

main :: IO ()
main = do
  content <- readFile "inputs/day-12.txt"
  let caveSystem = parseInput content
  putStrLn "Part 1"
  print $ length $ findAllPaths caveSystem
  putStrLn "Part 2"
  print $ length $ findMorePaths caveSystem

parseInput :: String -> CaveSystem
parseInput = foldr parsePath H.empty . lines

parsePath :: String -> CaveSystem -> CaveSystem
parsePath s c = let
  c1:c2:_ = asCave <$> splitOn "-" s
  in insertPath c1 c2 c

asCave :: String -> Cave
asCave "start" = Start
asCave "end" = End
asCave s@(c:_)
  | isUpper c = Big s
  | otherwise = Small s

insertPath :: Cave -> Cave -> CaveSystem -> CaveSystem
insertPath c1 c2 = H.alter (doInsert c2) c1 . H.alter (doInsert c1) c2

doInsert :: Cave -> Maybe CaveSet -> Maybe CaveSet
doInsert c Nothing   = Just $ S.singleton c
doInsert c (Just cs) = Just $ S.insert c cs

findAllPaths :: CaveSystem -> [[Cave]]
findAllPaths cs = explore cs (True, S.empty) Start

findMorePaths :: CaveSystem -> S.HashSet [Cave]
findMorePaths cs = S.fromList $ explore cs (False, S.empty) Start

explore :: CaveSystem -> Exclusion -> Cave -> [[Cave]]
explore _ _ End = [[End]]
explore _ (_, ex) Start
  | S.member Start ex = []
explore _ (True, ex) c
  | S.member c ex = []
explore cs e c = (c:) <$> concatMap (explore cs (exclude e c)) (cs ! c)

exclude :: Exclusion -> Cave -> Exclusion
exclude (b, cs) Start = (b, S.insert Start cs)
exclude (b, cs) c@(Small _) = (b || S.member c cs, S.insert c cs)
exclude e _ = e
