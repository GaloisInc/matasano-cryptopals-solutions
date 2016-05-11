module Set2.C9 where


import Set1.C1 hiding ( isCorrect,main )
import Set1.C5 hiding ( isCorrect,main )

isCorrect :: Bool
isCorrect = actual == expected
expected = unasciify "YELLOW SUBMARINE\x04\x04\x04\x04"
actual = pkcs_7Pad 20 (unasciify "YELLOW SUBMARINE")

pkcs_7Pad :: Int -> [Byte] -> [Byte]
pkcs_7Pad blockLen bs = bs ++ padding
  where
  totalLen = length bs
  padLen = blockLen - (totalLen `mod` blockLen)
  padding = replicate padLen (fromIntegral padLen)
