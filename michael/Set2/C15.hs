{-# LANGUAGE NoMonomorphismRestriction #-}
module Set2.C15 where

import Data.List (last)
import qualified Test.QuickCheck as QC

import Set1 hiding ( ciphertext )
import Set2.C9 hiding ( main )

pkcs_7Unpad_safe :: [Byte] -> Maybe [Byte]
pkcs_7Unpad_safe bs = if and (map (==pad_len) padding)
                         then Just (take len bs)
                         else Nothing
  where
  pad_len = fromIntegral (last bs)
  len = length bs - pad_len
  padding = drop len bs

main :: IO ()
main = do
  QC.quickCheck prop_pad_unpad_id

prop_pad_unpad_safe_id :: [Byte] -> QC.Property
prop_pad_unpad_safe_id bs =
  QC.forAll (QC.choose (1,64)) $ \blockLen ->
    Just bs == pkcs_7Unpad_safe (pkcs_7Pad blockLen bs)

