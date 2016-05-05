module C3 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char
import Data.List (sortOn)
import Data.Word (Word8)

import C1 hiding (isCorrect)

ciphertext :: [Char]
ciphertext = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

candidates :: [Char] -> [String]
candidates input = do
  key <- enumFromTo (minBound :: Word8) maxBound
  let cipherBytes = decodeHex input
      plainBytes = zipWith (xor) cipherBytes (repeat key)
      plaintext = map (toEnum . fromIntegral) plainBytes
  guard (and $ map isAscii plaintext)
  return plaintext

rankedCandidates :: [Char] -> [String]
rankedCandidates input = sortOn score (candidates input)

findHighestRank = head . rankedCandidates
highestRank = findHighestRank ciphertext

score :: String -> Int
score = sum . map f
  where
  f :: Char -> Int
  f c = if c `elem` common
           then 0
           else 1


common :: [Char]
common = concat $ zipWith (\x y -> [x,y]) ls (map toLower ls)
  where
  ls = "ETAOIN SHRDLU"
