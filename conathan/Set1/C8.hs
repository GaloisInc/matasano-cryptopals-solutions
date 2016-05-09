{-
### Detect AES in ECB mode

[In this file](/static/challenge-data/8.txt) are a bunch of hex-encoded
ciphertexts.

One of them has been encrypted with ECB.

Detect it.

Remember that the problem with ECB is that it is stateless and
deterministic; the same 16 byte plaintext block will always produce the
same 16 byte ciphertext.
-}

{-
Don't see how to be sure of completing this exercise. Rather, I'll
just look for a hex string with a repeating 16-byte block ...

With my scoring function below, only one ciphertext has non-zero
score:

Score: 0.40
Ciphertext: d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a

-}

module Set1.C8 where

import qualified Data.Map.Strict as Map

import Common
import Set1.C1 hiding ( main )
import Set1.C6 ( chunks )

-- | The score is the proportion of blocks that are duplicates.
scoreEcbLikelihood :: Int -> Raw -> Double
scoreEcbLikelihood bytesPerBlock ciphertext = score
  where
  score =
    (sum . filter (> 1) $ Map.elems counts) /
    (fromIntegral $ length chunks_)
  counts = foldl' countChunk Map.empty chunks_
  chunks_ = chunks bytesPerBlock ciphertext
  countChunk map_ chunk = Map.alter (Just . maybe (1::Double) (+1)) chunk map_

main :: IO ()
main = do
  ciphertexts <- map base16ToRaw . lines <$> readFile "Set1/8.txt"
  let rankedCiphertexts =
        [ (scoreEcbLikelihood (128 `div` 8) c, c) | c <- ciphertexts ]
  forM_ (reverse . sort $ rankedCiphertexts) $ \(score, ciphertext) -> do
    printf "Score: %0.2f\nCiphertext: %s\n" score (rawToBase16 ciphertext)
