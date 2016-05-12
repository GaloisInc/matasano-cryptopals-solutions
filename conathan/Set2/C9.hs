{-
### Implement PKCS\#7 padding

A block cipher transforms a fixed-sized block (usually 8 or 16 bytes) of
plaintext into ciphertext. But we almost never want to transform a
single block; we encrypt irregularly-sized messages.

One way we account for irregularly-sized messages is by padding,
creating a plaintext that is an even multiple of the blocksize. The most
popular padding scheme is called PKCS\#7.

So: pad any block to a specific block length, by appending the number of
bytes of padding to the end of the block. For instance,

    "YELLOW SUBMARINE"

... padded to 20 bytes would be:

    "YELLOW SUBMARINE\x04\x04\x04\x04"
-}

{-

-}

module Set2.C9 where

import qualified Test.QuickCheck as QC

import Common
import Set1

-- | To avoid ambiguity, pkcs#7 padding is defined so that if the
-- final block of plaintext is full, then an entire block of padding
-- is added. In other words, there is always padding at the end, so
-- unpadding is not ambiguous.
--
-- The input 'raw' here is the full plaintext, not the final block.
pkcs7Pad :: Int -> Raw -> Raw
pkcs7Pad blockSize plaintext = plaintext <> replicate padValue padValue
  where
  padValue :: Integral n => n
  padValue = fromIntegral $ blockSize - (length plaintext `mod` blockSize)

-- | Assumes input is the output of 'pkcs7Pad' and hence is non-empty.
pkcs7Unpad :: Raw -> Raw
pkcs7Unpad [] = error "pkcs7Unpad: empty input!"
pkcs7Unpad plaintext =
  take (length plaintext - fromIntegral (last plaintext)) plaintext

prop_pkcs7_round_trip =
  QC.forAll QC.arbitrary $ \blockSize ->
    QC.forAll QC.arbitrary $ \in_ ->
      QC.collect ("(blocksize, length in_)", (blockSize, length in_)) $
      QC.classify (blockSize <= length in_) "multiple blocks" $
      (blockSize > 0) QC.==> in_ == pkcs7Unpad (pkcs7Pad blockSize in_)

main :: IO ()
main = do
  forM_ tests $ \(blockSize, in_, out) -> do
    let padded = pkcs7Pad blockSize (stringToRaw in_)
    printf "Padded value:\n%s\n" (show (rawToString padded))
    printf "Equal to %s?\n%s\n" (show out) (show $ padded == stringToRaw out)
    let unpadded = rawToString $ pkcs7Unpad padded
    printf "Unpadded equal to original?\n%s\n" (show $ unpadded == in_)
    printf "\n"

  QC.quickCheck prop_pkcs7_round_trip
  where
  tests =
    [ ( 20
      , "YELLOW SUBMARINE"
      , "YELLOW SUBMARINE" ++ replicate 4 '\x04'
      )
    , ( 16
      , "YELLOW SUBMARINE"
      , "YELLOW SUBMARINE" ++ replicate 16 '\x10'
      )
    , ( 10
      , ""
      , replicate 10 '\x0a'
      )
    ]
