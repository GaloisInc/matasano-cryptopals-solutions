{-

### Break fixed-nonce CTR statistically

[In this file](/static/challenge-data/20.txt) find a similar set of
Base64'd plaintext. Do with them exactly what you did with the first,
but solve the problem differently.

Instead of making spot guesses at to known plaintext, treat the
collection of ciphertexts the same way you would repeating-key XOR.

Obviously, CTR encryption appears different from repeated-key XOR, *but
with a fixed nonce they are effectively the same thing.*

To exploit this: take your collection of ciphertexts and truncate them
to a common length (the length of the smallest ciphertext will work).

Solve the resulting concatenation of ciphertexts as if for repeating-
key XOR, with a key size of the length of the ciphertext you XOR'd.

-}

module Set3.C20 () where

import Data.List ( group )

import Common
import Set1
import Set2
import Set3.C19

mode :: (Ord a, Eq a) => [a] -> a
mode xs =
  snd . head . reverse . sort $ [ (length g, head g) | g <- groups ]
  where
  groups = group . sort $ xs

solveUsingC19 :: [Base64] -> IO ()
solveUsingC19 plainTexts = do
  print key
  -- solveRepeatedNonceCtrMode key rawCipherTexts
  where
    guesses = map (guessKeyByte englishLikeness) rawCipherTextColumns
    key = [ k | (_score, k, _decryption) <- map head guesses ]
    rawCipherTextColumns = transpose rawCipherTexts
    rawCipherTexts = map encryptC19 plainTexts

solveUsingC6 :: [Base64] -> IO ()
solveUsingC6 plainTexts = do
  let (score, key, plainText) = head $
        solveMultiCharXor englishLikeness [minLen] cipherText
  print $ stringToRaw key
  mapM_ putStrLn $ chunks minLen plainText
  where
  cipherTexts = map encryptC19 plainTexts
  minLen = minimum . map length $ cipherTexts
  cipherText = concat . map (take minLen) $ cipherTexts

mainUsingC6 :: IO ()
mainUsingC6 = solveUsingC6 =<< plainTextsC20

mainUsingC19 :: IO ()
mainUsingC19 = solveUsingC19 =<< plainTextsC20

plainTextsC20 :: IO [Base64]
plainTextsC20 = do
  lines <$> readFile "Set3/20.txt"

