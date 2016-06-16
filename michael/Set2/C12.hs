module Set2.C12 where

import Data.List.Split ( chunksOf )
import System.Random

import Set1
import Set2.C9 hiding ( main )
import Set2.C10 hiding ( main, prop_encrypt_decrypt_id, runTests )
import Set2.C11 hiding ( main, prop_encrypt_decrypt_id )

footer :: [Byte]
footer = decodeBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

footerAscii :: String
footerAscii = "Rollin' in my 5.0\nWith my rag-top down so my hair can blow\nThe girlies on standby waving just to say hi\nDid you stop? No, I just drove by\n"

randomECB :: StdGen -> [Byte] -> [Byte]
randomECB g bs = do
  let key = take 16 (randoms g)
   in myECBEncrypt' key (bs ++ footer)

-- This function applies the encryption function to a successively longer
-- list of 0's, then checks when the ciphertext length jumps up a block.
-- The jump in size tells us the block size, and from that (and the length
-- of our input message) we can find the footer length.
discoverBlockSizeAndFooterLength :: ([Byte] -> [Byte]) -> (Int,Int)
discoverBlockSizeAndFooterLength enc = go $ zip lengths [0..]
  where
  bss = map (flip replicate 0) [0..]
  lengths = map (length . enc) bss
  go ((x,xi):(y,yi):rest) = if x == y
                               then go ((y,yi):rest)
                               else (y - x, x - xi - 1)

getEnc :: IO ([Byte] -> [Byte])
getEnc = randomECB <$> getStdGen

-- (padLen, blockIdx) where padLen is the number of bytes to pad the input
-- with, and blockIdx is the index of the cipher block we are concerned with.
getIndices :: Int -> [(Int,Int)]
getIndices blockSize = map f [0..]
  where
  f i = let (d,m) = divMod i blockSize
         in (blockSize - 1 - m, d)

decryptFooter :: Int -> Int -> ([Byte] -> [Byte]) -> [Byte]
decryptFooter blockSize footerLen enc = foldl go [] indices
  where
  indices = take footerLen (getIndices blockSize)
  go footerSoFar (padLen,blockIdx) =
    case filter ((== targetBlock) . snd) candidates of
      [] -> error "no candidates matched"
      [(b,_)] -> footerSoFar ++ [b]
      z -> error $ show (length z) ++ " candidates matched. " ++ show (padLen,blockIdx)
    where
    padding = replicate padLen 0
    ciphertext = enc padding
    chunks = chunksOf blockSize ciphertext
    targetBlock = chunks !! blockIdx
    candidates :: [(Byte,[Byte])]
    candidates = do
      b <- enumFromTo (minBound :: Byte) maxBound
      let ciphertext = enc (padding ++ footerSoFar ++ [b])
          chunks = chunksOf blockSize ciphertext
      return (b, chunks !! blockIdx)

main :: IO ()
main = do
  enc <- getEnc
  let (blockSize, footerLen) = discoverBlockSizeAndFooterLength enc
  putStrLn $ "block size: " ++ show blockSize
  putStrLn $ "containsRepeat says: " ++ show (containsRepeat enc)
  let footerBytes = decryptFooter blockSize footerLen enc
      footerAscii' = asciify footerBytes
  putStrLn $ "Decrypted output: " ++ show footerAscii'
  putStrLn $ "Is correct? " ++ show (footerAscii' == footerAscii)

