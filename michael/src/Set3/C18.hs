module Set3.C18 where

import Data.Bits (xor)
import Data.List.Split ( chunksOf )

import Set1 ( Byte, unasciify )
import Set1.C7 ( myECBEncrypt )
import Set2.Helpers ( xor' )


aesCTRMode :: [Byte] -> [Byte] -> [Byte]
aesCTRMode nonce key =
  case (length nonce, length key) of
    (8,16) -> go 0
    _      -> error "nonce must be 8 bytes; key must be 16 bytes"
  where
    go ctr = myECBEncrypt key (nonce ++ integerToBytesLE 8 ctr)
          ++ go (ctr+1)

aesCTRModeGo :: [Byte] -> [Byte] -> [Byte] -> [Byte]
aesCTRModeGo nonce key bs = xor' keystream bs
  where
    keystream = take (length bs) (aesCTRMode nonce key)

integerToBytesLE :: Int -> Integer -> [Byte]
integerToBytesLE len n = take len (go n ++ repeat 0)
  where
    go n = let (q,r) = n `quotRem` 256
            in (fromIntegral r) : go q

