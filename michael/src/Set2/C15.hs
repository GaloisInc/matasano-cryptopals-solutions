{-# LANGUAGE NoMonomorphismRestriction #-}
module Set2.C15 where

import Data.List (last)

import Set1 hiding ( ciphertext )
import Set2.C9 hiding ( main )

pkcs_7Unpad_safe :: [Byte] -> Maybe [Byte]
pkcs_7Unpad_safe [] = Just []
pkcs_7Unpad_safe bs =
  if (pad_len > 0 &&
      and (map (==pad_len) padding))
     then Just (take len bs)
     else Nothing
  where
  pad_len = fromIntegral (last bs)
  len = length bs - pad_len
  padding = drop len bs

