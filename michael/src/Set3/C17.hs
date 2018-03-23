module Set3.C17 where

import Data.List.Split ( chunksOf )
import System.Random

import Set1 ( Byte, unasciify )
import Set2.C9  ( pkcs_7Pad )
import Set2.C10 ( myCBCEncrypt, myCBCDecryptPadded )
import Set2.C15 ( pkcs_7Unpad_safe )
import Util.MonadRandom


import Debug.Trace

stuffGo seed = flip evalRand (mkStdGen seed) $ do
  str <- randomElem possibleStrings
  key <- replicateM 16 getRandom
  iv  <- replicateM 16 getRandom
  let enc = myCBCEncrypt iv key
      dec = myCBCDecryptPadded iv key
      paddedBytes = pkcs_7Pad 16 (unasciify str)
      oracle ciphertext =
        case pkcs_7Unpad_safe (dec ciphertext) of
          Nothing -> False
          Just _  -> True
  return (enc paddedBytes, oracle, str)

paddingOracleAttack :: [Byte] -> ([Byte] -> Bool) -> String
paddingOracleAttack ciphertext oracle =
  trace (show (go [] 0)) "fishes"
  where
    ctChunks = chunksOf 16 ciphertext
    numCTChunks = length ctChunks
    ultimateCT = ctChunks !! (numCTChunks - 1)
    penultimateCT = ctChunks !! (numCTChunks - 2)
    --
    go :: [Byte] -> Byte -> [Byte]
    go acc v
      | length acc == 1 = acc
      | otherwise = if oracle (replicate (16 - length acc - 1) 0 ++ [v] ++ acc ++ ultimateCT)
                       then go (acc ++ [v]) 0
                       else go acc (v+1)


possibleStrings :: [String]
possibleStrings =
  [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
  , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
  , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
  , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
  , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
  , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
  , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
  , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
  , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
  , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
  ]
