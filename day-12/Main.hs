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
findAllPaths cs = explore (True, S.singleton Start) cs Start

findMorePaths :: CaveSystem -> S.HashSet [Cave]
findMorePaths cs = S.fromList $ explore (False, S.singleton Start) cs Start

-- This method is finding duplicates for the cases where we actually don't
-- visit any small cave twice. Luckily immutable lists are hashable so we can
-- dedup through a set.
explore :: Exclusion -> CaveSystem -> Cave -> [[Cave]]
explore _ _ End = [[End]]
explore eo cs c = (c:) <$> exploreOptions cs c (exclude c eo)

exploreOptions :: CaveSystem -> Cave -> [Exclusion] -> [[Cave]]
exploreOptions cs c = concatMap (exploreOption cs c)

exploreOption :: CaveSystem -> Cave -> Exclusion -> [[Cave]]
exploreOption cs c o = exploreCaves o cs (adjacentCaves cs c o)

exploreCaves :: Exclusion -> CaveSystem -> [Cave] -> [[Cave]]
exploreCaves e cs = concatMap (explore e cs)

adjacentCaves :: CaveSystem -> Cave -> Exclusion -> [Cave]
adjacentCaves cs c = S.toList . S.difference (cs ! c) . snd

exclude :: Cave -> Exclusion -> [Exclusion]
exclude c@(Small _) (False, cs) = [(True, cs), (False, S.insert c cs)]
exclude c@(Small _) (True, cs) = [(True, S.insert c cs)]
exclude _ fe = [fe]
