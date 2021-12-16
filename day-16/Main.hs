module Main where

import           Data.Bifunctor
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Word
import           Debug.Trace

data SubpacketNum = TotalLength Word | Count Word
  deriving Show

data Op = Sum | Product | Min | Max | Gt | Lt | Eq | Lit
  deriving Show

data Header = Header
  { version :: Word
  , op      :: Op
  } deriving Show

data Packet = Packet
  { header :: Header
  , body   :: PacketBody
  } deriving Show

data PacketBody = Val Word | Subpackets [Packet]
  deriving Show

type Bits' = [Word8]

main :: IO ()
main = do
  content <- readFile "inputs/day-16.txt"
  let packet = parse content
  putStrLn "Part 1"
  print $ sumVersions packet
  putStrLn "Part 2"
  print $ calculate packet

parse :: String -> Packet
parse = snd . readPacket . bits

bits :: String -> Bits'
bits ['\n'] = []
bits (c:str) = let
  val = fromHex c
  in [ shiftR val 3 .&. 1
     , shiftR val 2 .&. 1
     , shiftR val 1 .&. 1
     , val .&. 1
     ] ++ bits str

takeBits :: Bits' -> Int -> (Bits', Word)
takeBits b s = let
  (bits, b') = splitAt s b
  res = foldl' (\acc b -> shiftL acc 1 + fromIntegral b :: Word) 0 bits
  in (b', res)

fromHex :: Char -> Word8
fromHex    = fromIntegral . digitToInt

readPacket :: Bits' -> (Bits', Packet)
readPacket b = let
  (b', header) = readHeader b
  (b'', body) = case op header of
    Lit -> readVal b' 0
    _   -> readSubpacketsPacket b'
  in (b'', Packet { header = header, body = body })

readHeader :: Bits' -> (Bits', Header)
readHeader b = let
  (b', v) = takeBits b 3
  (b'', t) = second opFromInt $ takeBits b' 3
  in (b'', Header { version = v, op = t })

readVal :: Bits' -> Word -> (Bits', PacketBody)
readVal b buf = let
  (b', v) = takeBits b 5
  val = shiftL buf 4 + clearBit v 4
  in if testBit v 4
  then readVal b' val
  else (b', Val val)

opFromInt :: Word -> Op
opFromInt 0 = Sum
opFromInt 1 = Product
opFromInt 2 = Min
opFromInt 3 = Max
opFromInt 4 = Lit
opFromInt 5 = Gt
opFromInt 6 = Lt
opFromInt 7 = Eq

readSubpacketsPacket :: Bits' -> (Bits', PacketBody)
readSubpacketsPacket b = let
  (b', subpacketNum) = readSubpacketNum b
  (b'', packets) = readPackets b' subpacketNum
  in (b'', Subpackets packets)

readSubpacketNum :: Bits' -> (Bits', SubpacketNum)
readSubpacketNum b = let
  (b', v) = takeBits b 1
  in if v == 0
    then second TotalLength $ takeBits b' 15
    else second Count $ takeBits b' 11

readPackets :: Bits' -> SubpacketNum -> (Bits', [Packet])
readPackets b (Count 0) = (b, [])
readPackets b (TotalLength 0) = (b, [])
readPackets b s = let
  (b', packet) = readPacket b
  (b'', rest) = readPackets b' (decrement b b' s)
  in (b'', packet:rest)

decrement :: Bits' -> Bits' -> SubpacketNum -> SubpacketNum
decrement _ _ (Count n)         = Count (n - 1)
decrement b b' (TotalLength n) = TotalLength $ n - fromIntegral (length b - length b')

sumVersions :: Packet -> Word
sumVersions p = version (header p) + case body p of
  Val _         -> 0
  Subpackets ps -> sum $ sumVersions <$> ps

calculate :: Packet -> Word
calculate Packet { header = Header { op = Lit }, body = Val n } = n
calculate Packet { header = Header { op = op }, body = Subpackets n } =
  apply op $ calculate <$> n

apply :: Op -> [Word] -> Word
apply Sum w = sum w
apply Product w = product w
apply Min w = minimum w
apply Max w = maximum w
apply Gt [a, b]
  | a > b = 1
  | otherwise = 0
apply Lt [a, b]
  | a < b = 1
  | otherwise = 0
apply Eq [a, b]
  | a == b = 1
  | otherwise = 0
