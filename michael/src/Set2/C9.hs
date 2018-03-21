module Set2.C9 where

import Data.List (last)

import Set1

pkcs_7Pad :: Int -> [Byte] -> [Byte]
pkcs_7Pad blockLen bs = bs ++ padding
  where
  totalLen = length bs
  padLen = blockLen - (totalLen `mod` blockLen)
  padding = replicate padLen (fromIntegral padLen)


pkcs_7Unpad :: [Byte] -> [Byte]
pkcs_7Unpad bs = take len bs
  where
  len = length bs - (fromIntegral (last bs))

