module Set1.C3 where

import Common
import Set1.C1 hiding ( main )
import Set1.C2 hiding ( main )

-- Return ranked encryptions, sorted decreasing by score.
rankSingleCharXors :: (String -> Double) -> Raw -> [(Double, Word8, String)]
rankSingleCharXors rank raw = reverse . sort $
  [ (score, key, ascii)
  | key <- [0..255]
  , plainText <- [zipWith xor raw (repeat key)]
  , ascii <- [rawToString plainText]
  , score <- [rank ascii]
  ]

rankC3 :: String -> Double
rankC3 ascii = fromIntegral . length $ filter isGood ascii
  where
  -- A character is "good" if it's ASCII and alphanumeric, roughly.
  isGood c = isAscii c && isPrint c && not (isPunctuation c) && not (isSymbol c)

-- Solution:
{-
Cooking MC's like a pound of bacon
-}
-- Top scoring decryptions:
{-
Score = 33, key = 88, plaintext =
Cooking MC's like a pound of bacon

Score = 29, key = 79, plaintext =
Txx|~yp7ZT0d7{~|r7v7gxbys7xq7uvtxy

Score = 29, key = 78, plaintext =
Uyy}xq6[U1e6z}s6w6fycxr6yp6twuyx

Score = 28, key = 65, plaintext =
Zvvrpw~9TZ>j9upr|9x9ivlw}9v9{xzvw

Score = 27, key = 120, plaintext =
cOOKINGmcSLIKEAPOUNDOFBACON
-}
main :: IO ()
main = do
  forM_ (take 5 $ rankSingleCharXors rankC3 (base16ToRaw base16)) $ \(score, key, ascii) -> do
    printf "Score = %i, key = %i, plaintext =\n%s\n\n"
      score key ascii
  where
  base16 = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
