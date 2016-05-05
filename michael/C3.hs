module C3 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char (isPrint)
import Data.Word (Word8)

import C1 hiding (isCorrect)

ciphertext :: [Char]
ciphertext = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

candidates :: [String]
candidates = do
  key <- enumFromTo (minBound :: Word8) maxBound
  let cipherBytes = decodeHex ciphertext
      plainBytes = zipWith (xor) cipherBytes (repeat key)
      plaintext = map (toEnum . fromIntegral) plainBytes
  guard (and $ map isPrint plaintext)
  return plaintext
