module Set1.C5 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char
import Data.List (sortOn)
import Data.Word (Word8)
import System.IO

import Set1.C1
import Set1.C3

key :: [Byte]
key = cycle (map (fromIntegral . fromEnum) "ICE")

unasciify :: String -> [Byte]
unasciify = map (fromIntegral . ord)

asciify :: [Byte] -> String
asciify = map (chr . fromIntegral)

iceify :: [Byte] -> [Byte]
iceify = zipWith (xor) key

main :: IO ()
main = do
  msg <- hGetContents stdin
  putStrLn . encodeHex . iceify . unasciify $ msg
