{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Hashable
import           Debug.Trace
import qualified Lib
import           GHC.Generics        (Generic)

data Amphipod = A | B | C | D deriving (Eq, Show, Generic)
instance Hashable Amphipod

type Room = [Amphipod]
type Rooms = (Room, Room, Room, Room)
type Hallway = [Maybe Amphipod]

type State = (Rooms, Hallway)
type Cache = H.HashMap State (Maybe Int)

emptyHallway :: Hallway
emptyHallway = replicate 7 Nothing

main :: IO ()
main = do
  let rooms = ( [C, C]
              , [A, A]
              , [B, D]
              , [D, B]
              )
  let startingState = (rooms, emptyHallway)

  putStrLn "Part 1"
  let roomEnergy = energy 3 A + energy 3 B + energy 3 C + energy 3 D
  print $ Just (exitEnergy rooms + roomEnergy +)
    <*> snd (findMinEnergy H.empty startingState)

  putStrLn "Part 2"
  let bigRooms = ( [C, D, D, C]
                 , [A, C, B, A]
                 , [B, B, A, D]
                 , [D, A, C, B]
                 )
  let bigRoomEnergy = energy 10 A + energy 10 B + energy 10 C + energy 10 D
  let bigStartingState = (bigRooms, emptyHallway)
  print $ Just (exitEnergy bigRooms + bigRoomEnergy +)
    <*> snd (findMinEnergy H.empty bigStartingState)

exitEnergy :: Rooms -> Int
exitEnergy (ra, rb, rc, rd) = sum $ roomExitEnergy 1 <$> [ra, rb, rc, rd]

roomExitEnergy :: Int -> Room -> Int
roomExitEnergy _ [] = 0
roomExitEnergy d (b:rst) = energy d b + roomExitEnergy (d + 1) rst

findMinEnergy :: Cache -> State -> (Cache, Maybe Int)
findMinEnergy c s | H.member s c = (c, c ! s)
findMinEnergy c s@((ra, rb, rc, rd ), h)
  | all isNothing h
    && all (A ==) ra
    && all (B ==) rb
    && all (C ==) rc
    && all (D ==) rd
  = (H.insert s (Just 0) c, Just 0)
findMinEnergy c s = let
  (c', min) = case allMoves s of
          [] -> (c, Nothing)
          nextMoves -> foldl'
            (\(c', min) (s', e) -> let
                (c'', min') = findMinEnergy c' s'
                in (c'', somethingMin min (Just (e +) <*> min'))
            ) (c, Nothing) nextMoves
  in (H.insert s min c', min)

somethingMin :: Maybe Int -> Maybe Int -> Maybe Int
somethingMin Nothing a = a
somethingMin a Nothing = a
somethingMin a b = min a b

rangeFree :: Hallway -> [Int] -> Bool
rangeFree h r =
  all isNothing $ (h !!) <$> r

putAt :: Int -> Hallway -> Maybe Amphipod -> Hallway
putAt 0 (_:rst) b = b : rst
putAt n (s:rst) b = s : putAt (n - 1) rst b

allMoves :: State -> [(State, Int)]
allMoves s@((ra, rb, rc, rd), h) =
  moveFromHallway s ++
  ((\(r, h, e) -> (((r, rb, rc, rd), h), e)) <$> moveFromRoom A ra h) ++
  ((\(r, h, e) -> (((ra, r, rc, rd), h), e)) <$> moveFromRoom B rb h) ++
  ((\(r, h, e) -> (((ra, rb, r, rd), h), e)) <$> moveFromRoom C rc h) ++
  ((\(r, h, e) -> (((ra, rb, rc, r), h), e)) <$> moveFromRoom D rd h)

moveFromRoom :: Amphipod -> Room -> Hallway -> [(Room, Hallway, Int)]
moveFromRoom t [] _ = []
moveFromRoom t r _ | all (t ==) r = []
moveFromRoom t (b:rst) h = (\(h, e) -> (rst,h,e)) <$> moveToHallway t h b

moveToHallway :: Amphipod -> Hallway -> Amphipod -> [(Hallway, Int)]
moveToHallway r h b = catMaybes $ moveToSlot r h b <$> [0..6]

moveToSlot :: Amphipod -> Hallway -> Amphipod -> Int -> Maybe (Hallway, Int)
moveToSlot r h b s =
  if rangeFree h (pathToSlot r s) && isNothing (h !! s)
  then Just (putAt s h (Just b), energy (distanceToSlot r s) b)
  else Nothing

moveFromHallway :: State -> [(State, Int)]
moveFromHallway s = catMaybes $ moveFromSlot s <$> [0..6]

moveFromSlot :: State -> Int -> Maybe (State, Int)
moveFromSlot (r, h) i = case h !! i of
  Nothing -> Nothing
  Just b ->
    if canMoveToRoom r b && rangeFree h (pathToSlot b i)
    then Just ((updateRooms r b, putAt i h Nothing), energy (distanceToSlot b i) b)
    else Nothing

updateRooms :: Rooms -> Amphipod -> Rooms
updateRooms (ra, rb, rc, rd) A = (A:ra, rb, rc, rd)
updateRooms (ra, rb, rc, rd) B = (ra, B:rb, rc, rd)
updateRooms (ra, rb, rc, rd) C = (ra, rb, C:rc, rd)
updateRooms (ra, rb, rc, rd) D = (ra, rb, rc, D:rd)

canMoveToRoom :: Rooms -> Amphipod -> Bool
canMoveToRoom (ra, _, _, _) A = null ra || all (A==) ra
canMoveToRoom (_, rb, _, _) B = null rb || all (B==) rb
canMoveToRoom (_, _, rc, _) C = null rc || all (C==) rc
canMoveToRoom (_, _, _, rd) D = null rd || all (D==) rd

distanceToSlot :: Amphipod -> Int -> Int
distanceToSlot A 0 = 2
distanceToSlot A 1 = 1
distanceToSlot A 2 = 1
distanceToSlot A n = 2 + distanceToSlot B n
distanceToSlot B 2 = 1
distanceToSlot B 3 = 1
distanceToSlot B n
  | n < 2 = 2 + distanceToSlot A n
  | n > 3 = 2 + distanceToSlot C n
distanceToSlot C 3 = 1
distanceToSlot C 4 = 1
distanceToSlot C n
  | n < 3 = 2 + distanceToSlot B n
  | n > 4 = 2 + distanceToSlot D n
distanceToSlot D 6 = 2
distanceToSlot D 5 = 1
distanceToSlot D 4 = 1
distanceToSlot D n = 2 + distanceToSlot C n

pathToSlot :: Amphipod -> Int -> [Int]
pathToSlot A n = if n < 2 then [n + 1..1] else [2..n - 1]
pathToSlot B n = if n < 3 then [n + 1..2] else [3..n - 1]
pathToSlot C n = if n < 4 then [n + 1..3] else [4..n - 1]
pathToSlot D n = if n < 5 then [n + 1..4] else [5..n - 1]

energy :: Int -> Amphipod -> Int
energy n A = n
energy n B = n * 10
energy n C = n * 100
energy n D = n * 1000
