module Set2.C14 where

import Control.Monad.Identity
import Data.Char
import Data.List (intercalate, delete)
import Data.List.Split hiding ( split )
import System.Random

import Set1 hiding ( ciphertext )
import Set2.C11 hiding ( main, prop_encrypt_decrypt_id )
import Set2.C12 hiding ( main, randomECB, getEnc )


{-

AES-128-ECB(random-prefix || attacker-controlled || target-bytes, random-key)
Goal: Decrypt target-bytes.

--------------------------------------------------------------------------------
-- Old thoughts ----------------------------------------------------------------
--------------------------------------------------------------------------------

This is like C12, but now there is a prefix (of random bytes) of a random length.
I don't know how to break it, so I will ramble and see if I come up with any ideas.

Before we relied on being able to control the alignment, in order to figure out
"where we were" in the ciphertext. One way to do this, which now occurs to me, is to
pass in 2 (or more, but 2 is enough) identical blocks to the oracle, many times. If
the two blocks are aligned on a 16-byte boundary, then our ciphertext will have two
identical blocks (which is otherwise very low probability). We can run the oracle many
times and throw away the results unless we see those identical blocks.

Using that to "filter" our results, we can then add more padding after those two
identical blocks (using different values to avoid confusion). Thus we can recreate
the notion of "control" of alignment.

--------------------------------------------------------------------------------
-- New thoughts ----------------------------------------------------------------
--------------------------------------------------------------------------------

After talking to Nathan I realized that the front padding is a fixed length. So
the strategy is to figure out the length of the front padding, and then pad (in
the "attacker-controlled" section to a block boundary. Then we can just drop the
first n blocks of output, which allows us to wrap the new oracle function and then
reuse C12's code. This seems not too hard :D

-}

randomECB :: StdGen -> [Byte] -> [Byte]
randomECB g bs =
  let rnds = randoms g :: [Byte]
      headerLen = fromIntegral (head rnds)
      header = take headerLen (drop 1 rnds)
      key = take 16 (drop (headerLen+1) rnds)
   in myECBEncrypt' key (header ++ bs ++ footer)

getEnc :: IO ([Byte] -> [Byte])
getEnc = randomECB <$> getStdGen

wrapHeaderEnc :: Int -> Int -> ([Byte] -> [Byte]) -> ([Byte] -> [Byte])
wrapHeaderEnc blockSize headerLen enc = enc'
  where
  padLen = blockSize - (headerLen `mod` blockSize)
  padding = replicate padLen 0
  enc' bytes = drop (headerLen + padLen) (enc (padding ++ bytes))

findHeaderLength :: StdGen -> Int -> ([Byte] -> [Byte]) -> Int
findHeaderLength g blockSize enc = go 0
  where
  randBlock = take 16 (randoms g)
  go padLen = let padding = replicate padLen 0
                  msg = padding ++ randBlock ++ randBlock
               in case duplicateBlockByteOffset blockSize (enc msg) of
                    Just offset -> offset - padLen
                    Nothing -> go (padLen + 1)

-- This looks for duplicate blocks and returns the offset (in bytes) of the
-- first block of two identical adjacent blocks (if such a pair exist).
duplicateBlockByteOffset :: Int -> [Byte] -> Maybe Int
duplicateBlockByteOffset blockSize bs = go 0 chunks
  where
  chunks = chunksOf blockSize bs
  go offset (b1:b2:rest) = if b1 == b2
                              then Just offset
                              else go (offset + blockSize) (b2:rest)
  go offset _ = Nothing

affirmate :: Bool -> String
affirmate True  = "Yes"
affirmate False = "No"

main :: IO ()
main = do
  g <- getStdGen
  let (g1,g2) = split g
      enc = randomECB g1
      -- This code found only the footer length in C12, but now we have a header
      -- so it finds the header length + footer length.
      (blockSize, headerPlusFooterLen) = discoverBlockSizeAndFooterLength enc
      headerLen = findHeaderLength g2 blockSize enc
      footerLen = headerPlusFooterLen - headerLen
      enc' = wrapHeaderEnc blockSize headerLen enc
  putStrLn $ "blockSize: " ++ show blockSize
  putStrLn $ "headerLen: " ++ show headerLen
  putStrLn $ "footerLen: " ++ show footerLen

  let footerBytes = decryptFooter blockSize footerLen enc'
      footerAscii' = asciify footerBytes
  putStrLn $ "Decrypted output: " ++ show footerAscii'
  putStrLn $ "Is correct? " ++ affirmate (footerAscii' == footerAscii)

