{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Char
import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Hashable
import           Data.List.Split
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
findAllPaths cs = explore cs (True, S.singleton Start) Start

findMorePaths :: CaveSystem -> S.HashSet [Cave]
findMorePaths cs = S.fromList $ explore cs (False, S.singleton Start) Start

-- This method is finding duplicates for the cases where we actually don't
-- visit any small cave twice. Luckily immutable lists are hashable so we can
-- dedup through a set.
explore :: CaveSystem -> Exclusion -> Cave -> [[Cave]]
explore _ _ End = [[End]]
explore cs eo c = (c:) <$> exploreOptions cs (exclude eo c) c

exploreOptions :: CaveSystem -> [Exclusion] -> Cave -> [[Cave]]
exploreOptions cs es c = concatMap (flip (exploreOption cs) c) es

exploreOption :: CaveSystem -> Exclusion -> Cave -> [[Cave]]
exploreOption cs e = exploreCaves cs e . adjacent cs e

exploreCaves :: CaveSystem -> Exclusion -> [Cave] -> [[Cave]]
exploreCaves cs = concatMap . explore cs

adjacent :: CaveSystem -> Exclusion -> Cave -> [Cave]
adjacent cs (_, e) c = S.toList $ S.difference (cs ! c) e

exclude :: Exclusion -> Cave -> [Exclusion]
exclude (False, cs) c@(Small _) = [(True, cs), (False, S.insert c cs)]
exclude (True, cs) c@(Small _)  = [(True, S.insert c cs)]
exclude e _                     = [e]
