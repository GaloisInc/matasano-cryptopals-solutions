module Set1.C3 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char
import Data.List (sortOn)
import Data.Word (Word8)

import Set1.C1 hiding (isCorrect)

ciphertext :: [Char]
ciphertext = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

candidates :: [Byte] -> [(Word8,String)]
candidates cipherBytes = do
  key <- enumFromTo (minBound :: Word8) maxBound
  let plainBytes = zipWith (xor) cipherBytes (repeat key)
      plaintext = map (toEnum . fromIntegral) plainBytes
  guard (and $ map isAscii plaintext)
  return (key,plaintext)

rankedCandidates :: [Byte] -> [(Word8,String)]
rankedCandidates input = sortOn (score . snd) (candidates input)

ranked = rankedCandidates (decodeHex ciphertext)

isCorrect :: Bool
isCorrect = snd (head ranked) == "Cooking MC's like a pound of bacon"

score :: String -> Double
score = sum . map f
  where
  f :: Char -> Double
  f c = case lookup c englishDistr of
          Nothing -> 0
          Just sc -> -sc

englishDistr :: [(Char,Double)]
englishDistr = concatMap (\p@(c,v) -> [p,(toLower c,v)]) pairs
  where
  pairs = [(' ',18.28846265)
          ,('E',10.26665037)
          ,('T',7.51699827)
          ,('A',6.53216702)
          ,('O',6.15957725)
          ,('N',5.71201113)
          ,('I',5.66844326)
          ,('S',5.31700534)
          ,('R',4.98790855)
          ,('H',4.97856396)
          ,('L',3.31754796)
          ,('D',3.28292310)
          ,('U',2.27579536)
          ,('C',2.23367596)
          ,('M',2.02656783)
          ,('F',1.98306716)
          ,('W',1.70389377)
          ,('G',1.62490441)
          ,('P',1.50432428)
          ,('Y',1.42766662)
          ,('B',1.25888074)
          ,('V',0.79611644)
          ,('K',0.56096272)
          ,('X',0.14092016)
          ,('J',0.09752181)
          ,('Q',0.08367550)
          ,('Z',0.05128469)
          ]
