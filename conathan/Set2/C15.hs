{-

### PKCS\#7 padding validation

Write a function that takes a plaintext, determines if it has valid
PKCS\#7 padding, and strips the padding off.

The string:

    "ICE ICE BABY\x04\x04\x04\x04"

... has valid padding, and produces the result "ICE ICE BABY".

The string:

    "ICE ICE BABY\x05\x05\x05\x05"

... does not have valid padding, nor does:

    "ICE ICE BABY\x01\x02\x03\x04"

If you are writing in a language with exceptions, like Python or Ruby,
make your function throw an exception on bad padding.

Crypto nerds know where we're going with this. Bear with us.

-}
module Set1.C15 where

import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2.C9 ( pkcs7Pad, pkcs7Unpad )

-- | Returns nothing on bad padding, and 'error's on bad plaintext
-- length.
--
-- See 'Set2.C9.pkcs7Unpad' for a simpler, less paranoid version.
pkcs7SafeUnpad :: Int -> Raw -> Maybe Raw
pkcs7SafeUnpad blockSize plaintext =
  assert "pkcs7Unpad: plaintext length is positive and multiple of block size."
    ((length plaintext `mod` blockSize == 0) &&
     (length plaintext > 0))
    plaintext'
  where
  padLength = fromIntegral (last plaintext)
  padBytes = take padLength (reverse plaintext)
  plaintext' =
    if padLength > 0 &&
       all (== head padBytes) (tail padBytes) &&
       padLength <= blockSize
    then Just $ pkcs7Unpad plaintext
    else Nothing

----------------------------------------------------------------

prop_pkcs7SafeUnpad_roundTrip :: QC.Property
prop_pkcs7SafeUnpad_roundTrip =
  QC.forAll QC.arbitrary $ \plaintext ->
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
  pkcs7SafeUnpad blockSize (pkcs7Pad blockSize plaintext) == Just plaintext

prop_pkcs7SafeUnpad_rejectBadPadding :: QC.Property
prop_pkcs7SafeUnpad_rejectBadPadding =
  QC.once $ QC.conjoin
    [ pkcs7SafeUnpad blockSize (stringToRaw s) == Nothing
    | (blockSize, s) <- tests
    ]
  where
  tests =
    [ (16, "ICE ICE BABY\x05\x05\x05\x05")
    , (16, "ICE ICE BABY\x01\x02\x03\x04")
    , ( 2, "\x04\x04\x04\x04")
    ]

main :: IO ()
main = do
  QC.quickCheck prop_pkcs7SafeUnpad_roundTrip
  QC.quickCheck prop_pkcs7SafeUnpad_rejectBadPadding
