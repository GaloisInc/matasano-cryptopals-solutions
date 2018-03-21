module Set2.C10 where

import Data.Bits (xor)
import Data.Char
import Data.List.Split (chunksOf)

import Set1
import Set2.C9 hiding ( main )
import Set2.Helpers


aes128BlockLen :: Int
aes128BlockLen = 16

myCBCEncrypt :: [Byte] -> [Byte] -> [Byte] -> [Byte]
myCBCEncrypt iv key plaintext = concat cipherChunks
  where
  blockLen = if length iv /= length key
                then error "key/iv length mismatch"
                else length key
  chunks = chunksOf blockLen (pkcs_7Pad blockLen plaintext)
  cbcBlock acc block = ecbEnc (xor' acc block)
  ecbEnc = myECBEncrypt key
  -- tail is needed to drop the iv
  cipherChunks = tail (scanl cbcBlock iv chunks)

myCBCDecrypt :: [Byte] -> [Byte] -> [Byte] -> [Byte]
myCBCDecrypt iv key ciphertext = pkcs_7Unpad paddedPlaintext
  where
  blockLen = if length iv /= length key
                then error "key/iv length mismatch"
                else length key
  chunks = chunksOf blockLen ciphertext
  decChunks = map ecbDec chunks
  ecbDec = myECBDecrypt key
  paddedPlaintext = concat (zipWith (xor') decChunks (iv:chunks))

ivBytes :: [Byte]
ivBytes = replicate 16 0
keyBytes :: [Byte]
keyBytes = map (fromIntegral . ord) keyString
  where
  keyString = "YELLOW SUBMARINE"

main :: IO ()
main = do

  -- Decrypt file
  cipherBytes <- (decodeBase64 . concat . lines) <$> readFile "Set2/10.txt"
  let plainBytes = myCBCDecrypt ivBytes keyBytes cipherBytes
      plainAscii = map (chr . fromIntegral) plainBytes

  putStrLn "Now, the file:"
  putStrLn plainAscii


