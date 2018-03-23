module Set3.C17 where

import Data.Bits (xor)
import Data.List.Split ( chunksOf )
import System.Random

import Set1 ( Byte, unasciify )
import Set2.Helpers ( xor' )
import Set2.C9  ( pkcs_7Pad )
import Set2.C10 ( myCBCEncrypt, myCBCDecryptPadded )
import Set2.C15 ( pkcs_7Unpad_safe )
import Util.MonadRandom


stuffGo seed = flip evalRand (mkStdGen seed) $ do
  str <- randomElem possibleStrings
  key <- replicateM 16 getRandom
  iv  <- replicateM 16 getRandom
  let enc = myCBCEncrypt iv key
      dec = myCBCDecryptPadded iv key
      oracle ciphertext =
        case pkcs_7Unpad_safe (dec ciphertext) of
          Nothing -> False
          Just _  -> True
  return (enc (unasciify str), oracle, str, iv)

paddingOracleAttack :: [Byte] -> [Byte] -> ([Byte] -> Bool) -> [Byte]
paddingOracleAttack iv ciphertext oracle = concat allBlocks
  where
    ctChunks = chunksOf 16 ciphertext

    allBlocks :: [[Byte]]
    allBlocks = zipWith divineBlock (iv:ctChunks) ctChunks

    divineBlock :: [Byte] -> [Byte] -> [Byte]
    divineBlock ct0 ct1 = go [] 0
      where
      --
      go :: [Byte] -> Byte -> [Byte]
      go acc v
        | length acc == 16 = acc
        | otherwise = if oracle (replicate (16 - length acc - 1) 0 ++ [v] ++ xorAcc ++ ct1)
                         then let penByte = ct0 !! (16 - length acc - 1)
                                  paddingValue = fromIntegral (1 + length acc)
                                  divinedByte = v `xor` penByte `xor` paddingValue
                               in go (divinedByte : acc) 0
                         else go acc (v+1)
        where
          lenAcc = length acc
          paddingValue = 1 + fromIntegral lenAcc
          xorAcc = acc `xor'` (replicate lenAcc paddingValue) `xor'` (drop (16 - lenAcc) ct0)


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
