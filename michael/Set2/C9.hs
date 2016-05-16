module Set2.C9 where

import Data.List (last)
import qualified Test.QuickCheck as QC

import Set1

isCorrect :: Bool
isCorrect = actual == expected
expected = unasciify "YELLOW SUBMARINE\x04\x04\x04\x04"
actual = pkcs_7Pad 20 (unasciify "YELLOW SUBMARINE")

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

main :: IO ()
main = do
  QC.quickCheck prop_pad_unpad_id

prop_pad_unpad_id :: [Byte] -> QC.Property
prop_pad_unpad_id bs =
  QC.forAll (QC.choose (1,64)) $ \blockLen ->
    bs == pkcs_7Unpad (pkcs_7Pad blockLen bs)

