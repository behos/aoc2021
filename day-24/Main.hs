module Main where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           Lib

type Mem = (Int, Int, Int, Int)
type Input = [Int]
type State = (Mem, Input)
data Var = W | X | Y | Z deriving (Show, Eq)
data RHS = Var Var | Const Int deriving (Show, Eq)
data Op = Inp Var | Mul Var RHS | Add Var RHS | Div Var RHS | Mod Var RHS | Eql Var RHS
  deriving (Show, Eq)
type Program = [Op]

main :: IO ()
main = do
  program <- parseProgram
  putStrLn "Part 1"
  print $ maxModelNum program
  putStrLn "Part 2"
  print $ minModelNum program

parseProgram :: IO [Op]
parseProgram = do
  content <- readFile "inputs/day-24.txt"
  return $ parseLine <$> lines content

parseLine :: String -> Op
parseLine s = let
  (rawOp:fst:rst) = words s
  in case rawOp of
       "inp" -> Inp $ toVar fst
       "mul" -> Mul (toVar fst) (toRHS rst)
       "add" -> Add (toVar fst) (toRHS rst)
       "div" -> Div (toVar fst) (toRHS rst)
       "mod" -> Mod (toVar fst) (toRHS rst)
       "eql" -> Eql (toVar fst) (toRHS rst)

maxModelNum :: [Op] -> Int
maxModelNum p = maximum $ inputToInt <$> filter (runProgram p <$> ((0,0,0,0),)) allInputs

minModelNum :: [Op] -> Int
minModelNum p = minimum $ inputToInt <$> filter (runProgram p <$> ((0,0,0,0),)) allInputs

allInputs :: [[Int]]
allInputs = [
  [a, b, c, d, e, e + 7, g, g - 7, d - 2, j, j - 3, c - 8, b + 5, a]
  | a <- [1..9]
  , b <- [1..4]
  , c <- [9..9]
  , d <- [3..9]
  , e <- [1..2]
  , g <- [8..9]
  , j <- [4..9]
  ]

inputToInt :: [Int] -> Int
inputToInt []     = 0
inputToInt (i:is) = i * (10 ^ length is) + inputToInt is

runProgram :: Program -> State -> Bool
runProgram [] ((_, _, _, 0), _) = trace "got a result" True
runProgram [] s = False
runProgram (op:ops) s = case doOp s op of
  Just s' -> runProgram ops s'
  Nothing -> False

toVar :: String -> Var
toVar "w" = W
toVar "x" = X
toVar "y" = Y
toVar "z" = Z

toRHS :: [String] -> RHS
toRHS [] = error "missing rhs"
toRHS [a] | any isDigit a = Const $ toInt a
toRHS [a] = Var $ toVar a

get :: Mem -> Var -> Int
get (w, x, y, z) W = w
get (w, x, y, z) X = x
get (w, x, y, z) Y = y
get (w, x, y, z) Z = z

set :: Mem -> Var -> Int -> Mem
set (w, x, y, z) W v = (v, x, y, z)
set (w, x, y, z) X v = (w, v, y, z)
set (w, x, y, z) Y v = (w, x, v, z)
set (w, x, y, z) Z v = (w, x, y, v)

doOp :: State -> Op -> Maybe State
doOp (m, _) (Div _ r) | val m r == 0 = Nothing
doOp (m, _) (Mod v r) | get m v < 0 || val m r <= 0 = Nothing
doOp (m, i:rst) (Inp v) = Just (set m v i, rst)
doOp (m, i) (Mul v r) = Just (set m v (get m v * val m r), i)
doOp (m, i) (Add v r) = Just (set m v (get m v + val m r), i)
doOp (m, i) (Div v r) = Just (set m v (div (get m v) (val m r)), i)
doOp (m, i) (Mod v r) = Just (set m v (mod (get m v) (val m r)), i)
doOp (m, i) (Eql v r) = Just (set m v (if get m v == val m r then 1 else 0), i)

val :: Mem -> RHS -> Int
val _ (Const v) = v
val m (Var v)   = get m v
