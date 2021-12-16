module Main where

import           Data.Bifunctor
import           Data.Bits
import           Data.List
import           Data.Word
import           Debug.Trace

data SubpacketNum = TotalLength Word | Count Word
  deriving Show

data Op = Sum | Product | Min | Max | Gt | Lt | Eq | Lit
  deriving Show

data Header = Header
  { version    :: Word
  , op :: Op
  } deriving Show

data Packet = Packet
  { header :: Header
  , body   :: PacketBody
  } deriving Show

data PacketBody = Val Word | Subpackets [Packet]
  deriving Show

data BufParse = BufParse
  { buffer :: Maybe Word8
  , parsed :: [Word8]
  }

data Reader = Reader
  { content :: [Word8]
  , cursor  :: Word
  } deriving Show

main :: IO ()
main = do
  content <- readFile "inputs/day-16.txt"
  let packet = parse content
  putStrLn "Part 1"
  print $ sumVersions packet
  putStrLn "Part 2"
  print $ calculate packet

parse :: String -> Packet
parse = snd . readPacket . fromHexStr

fromHexStr :: String -> Reader
fromHexStr raw = Reader
  { content = allWords $
    foldl' bufParse (BufParse { buffer = Nothing, parsed = [] }) raw
  , cursor = 0
  }

allWords :: BufParse -> [Word8]
allWords b = parsed b ++ case buffer b of
  Nothing -> []
  Just w  -> [w]

bufParse :: BufParse -> Char -> BufParse
bufParse b '\n' = b
bufParse b@BufParse { buffer = Nothing, parsed = p } c =
  b { buffer = Just (shiftL (fromHex c) 4) }
bufParse b@BufParse { buffer = Just bf, parsed = p } c =
  b { buffer = Nothing, parsed = p ++ [bf + fromHex c] }

takeBits :: Reader -> Int -> (Reader, Word)
takeBits r 0 = (r, 0)
takeBits r s = let
  (r1, w8) = takeBit r
  w = fromIntegral w8 :: Word
  (r2, rest) = takeBits r1 (s - 1)
  in (r2, shiftL w (s - 1) + rest)

takeBit :: Reader -> (Reader, Word8)
takeBit r@Reader { content = (w:c), cursor = cr } = let
  offset = fromIntegral (mod cr 8) :: Int
  wd = shiftR w (7 - offset) .&. 1
  nxt = cr + 1
  in (case mod nxt 8 of
    0 -> r { content = c, cursor = nxt }
    _ -> r { cursor = nxt }, wd)

fromHex :: Char -> Word8
fromHex '0' = 0
fromHex '1' = 1
fromHex '2' = 2
fromHex '3' = 3
fromHex '4' = 4
fromHex '5' = 5
fromHex '6' = 6
fromHex '7' = 7
fromHex '8' = 8
fromHex '9' = 9
fromHex 'A' = 10
fromHex 'B' = 11
fromHex 'C' = 12
fromHex 'D' = 13
fromHex 'E' = 14
fromHex 'F' = 15

readPacket :: Reader -> (Reader, Packet)
readPacket r = let
  (r1, header) = readHeader r
  (r2, body) = case op header of
    Lit -> readVal r1 0
    _   -> readSubpacketsPacket r1
  in (r2, Packet { header = header, body = body })

readHeader :: Reader -> (Reader, Header)
readHeader r = let
  (r1, v) = takeBits r 3
  (r2, t) = second opFromInt $ takeBits r1 3
  in (r2, Header { version = v, op = t })

readVal :: Reader -> Word -> (Reader, PacketBody)
readVal r buf = let
  (r1, v) = takeBits r 5
  val = shiftL buf 4 + clearBit v 4
  in if testBit v 4
  then readVal r1 val
  else (r1, Val val)

opFromInt :: Word -> Op
opFromInt 0 = Sum
opFromInt 1 = Product
opFromInt 2 = Min
opFromInt 3 = Max
opFromInt 4 = Lit
opFromInt 5 = Gt
opFromInt 6 = Lt
opFromInt 7 = Eq

readSubpacketsPacket :: Reader -> (Reader, PacketBody)
readSubpacketsPacket r = let
  (r1, subpacketNum) = readSubpacketNum r
  (r2, packets) = readPackets r1 subpacketNum
  in (r2, Subpackets packets)

readSubpacketNum :: Reader -> (Reader, SubpacketNum)
readSubpacketNum r = let
  (r1, b) = takeBit r
  in if b == 0
    then second TotalLength $ takeBits r1 15
    else second Count $ takeBits r1 11

readPackets :: Reader -> SubpacketNum -> (Reader, [Packet])
readPackets r (Count 0) = (r, [])
readPackets r (TotalLength 0) = (r, [])
readPackets r s = let
  (r1, packet) = readPacket r
  (r2, rest) = readPackets r1 (decrement r r1 s)
  in (r2, packet:rest)

decrement :: Reader -> Reader -> SubpacketNum -> SubpacketNum
decrement _ _ (Count n)         = Count (n - 1)
decrement r1 r2 (TotalLength n) = TotalLength (n - (cursor r2 - cursor r1))

sumVersions :: Packet -> Word
sumVersions p = version (header p) + case body p of
  Val _   -> 0
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
