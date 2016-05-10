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

import Common
import Set1.C1 hiding ( main )

pkcs7Pad :: Int -> Raw -> Raw
pkcs7Pad blockSize raw = raw <> replicate padValue padValue
  where
  padValue :: Integral n => n
  padValue = fromIntegral $ blockSize - length raw

main :: IO ()
main = do
  let padded = pkcs7Pad 20 (stringToRaw "YELLOW SUBMARINE")
  printf "Padded value: %s\n" (rawToString padded)
  printf "Equal to \"YELLOW SUBMARINE\\x04\\x04\\x04\\x04\"? %s\n"
    (show $ padded == stringToRaw "YELLOW SUBMARINE\x04\x04\x04\x04")
