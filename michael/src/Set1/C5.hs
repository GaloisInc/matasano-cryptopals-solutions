module Set1.C5 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char
import Data.List (sortOn)
import Data.Word (Word8)
import System.IO

import Set1.C1 hiding (isCorrect)
import Set1.C3 hiding (isCorrect)

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
  putStrLn $ doEETT msg

isCorrect :: Bool
isCorrect = doEETT msg == out

doEETT = encodeHex . iceify . unasciify

msg = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

out = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
