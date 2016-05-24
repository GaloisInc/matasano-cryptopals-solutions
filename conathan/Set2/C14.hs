{-

### Byte-at-a-time ECB decryption (Harder)

Take your oracle function [from \#12.](/sets/2/challenges/12) Now
generate a random count of random bytes and prepend this string to every
plaintext. You are now doing:

    AES-128-ECB(random-prefix || attacker-controlled || target-bytes, random-key)

Same goal: decrypt the target-bytes.

### Stop and think for a second. {.panel-title}

What's harder than challenge \#12 about doing this? How would you
overcome that obstacle? The hint is: you're using all the tools you
already have; no crazy math is required.

Think "STIMULUS" and "RESPONSE".

-}

{-

I'm assuming the 'random-prefix' is fixed once and for all, not
generated fresh on every call to the encryption oracle.

Let 'p' be the prefix, 'a' be the attacker controlled text in the
middle, and 's' be the suffix. Then using the same "grow 'a' until a
block boundary is reached" approach from Challenge 12, I can learn

  length (p ++ s)

Next, growing my repeated-char-plaintext 'a' until the first time I
see two repeated ciphertext blocks -- to be robust against repeats in
the prefix 'p', I could change the repeat char and see where two
repeats change -- I learn that

  p ++ a

fills everything up to the two repeated blocks, and so

  length p = length (p ++ a) - length a

Hence I know 'length p', and so 'length s' follows, and the rest of
the attack is essentially C12.

In fact -- and I didn't realize this until I had copy-and-pasted C12
and adapted it :P -- it's more than essentially C12, it's reduced to
C12! I.e., just define a wrapped encryption oracle which pads out the
secret prefix to a block boundary, and then drops the corresponding
blocks from the resulting ciphertext. This yields an encryption oracle
which only appends a secret suffix, as in C12.

-}

module Set2.C14 () where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2.C9 hiding ( main )
import Set2.C11 hiding ( main )
import qualified Set2.C12 as C12

-- Generated randomly. I don't even know how long it is (but it's at
-- most 255 bytes).
secretPrefix :: Raw
secretPrefix =
  [ 110, 203, 207, 9, 211, 109, 236, 119, 76, 10, 221, 127, 32, 186, 26, 54, 94, 110, 193, 93, 165, 199, 39, 70, 186, 2, 198, 55, 18, 37, 86, 129, 240, 37, 138, 195, 146, 119, 58, 222, 129, 24, 93, 161, 210, 120, 215, 49, 230, 153, 142, 139, 96, 181, 209, 26, 183, 151, 81, 202, 188, 167, 75, 173, 205, 213, 112, 95, 76, 171, 62, 205, 195, 155, 110, 149, 19, 70, 198, 250, 223, 84, 133, 63, 9, 87, 89, 193, 238, 170, 139, 171, 81, 214, 88, 30, 171, 200, 126, 248, 115, 188, 197, 54, 87, 52, 204, 107, 122, 146, 101, 89, 231, 234, 152, 240, 65, 241, 177, 48, 155 ]

ecbEncryptionOracle :: Raw -> Raw
ecbEncryptionOracle plaintext =
  aes128EcbEncrypt C12.secretKey (pkcs7Pad 16 $
    secretPrefix ++ plaintext ++ C12.secretSuffix)

-- | Return the list of indices and values at which length-k repeats
-- of returned value start.
kRepeats :: Eq a => Int -> [a] -> [(Int, a)]
kRepeats k = go 0
  where
  go i xs | length xs < k = []
  go i (x:xs) =
    ( if all (== x) (take (k-1) xs)
      then [(i,x)]
      else []
    ) ++ go (i+1) xs

-- | Return list of indices where value in first assoc list is
-- different from value in second assoc list.
indicesOfChangedVals :: (Eq i, Eq a) => [(i,a)] -> [(i,a)] -> [i]
indicesOfChangedVals kvs1 kvs2 =
  [ i | (i, x1) <- kvs1, Just x2 <- [lookup i kvs2], x1 /= x2 ]

-- | Return the size of secret prefix.
--
-- We grow padding size until there is a two-block repeat at some
-- position which changes to a different two-block repeat at that
-- position when we change our pad byte. We change the pad byte to be
-- sure we're not detecting a repeat in the secret (but constant)
-- plaintext we don't control.
discoverPrefixSize :: Int -> (Raw -> Raw) -> Int
discoverPrefixSize blockSize encrypt = go 0
  where
  go padSize | padSize >= blockSize =
    error $ "discoverPrefixSize: padSize too large! " ++
            "BUG (or cipher is not ECB)!"
  go padSize =
    if not (null is)
    then head is * blockSize - padSize
    else go (padSize + 1)
    where
    plaintextSize = blockSize * 2 + padSize
    mkKvs byte =
      kRepeats 2 . chunks blockSize $ encrypt (replicate plaintextSize byte)
    is = indicesOfChangedVals (mkKvs 0) (mkKvs 1)

-- | Learn the implicit suffix attached to plaintexts by given ECB
-- encryption oracle.
--
-- This is a stronger version of the same function from C12. Here we
-- additionally allow for a fixed-length, secret prefix text being
-- prepended to the attacker controlled middle text.
--
-- We reduce this problem to C12, by learning the secret prefix length
-- and creating a derived encryption oracle that pads the secret
-- prefix out to a block boundary and drops those blocks.
breakEcbSuffix :: (Raw -> Raw) -> Raw
breakEcbSuffix encrypt = C12.breakEcbSuffix encrypt'
  where
  (blockSize, _secretSize) = C12.discoverBlockAndSecretSize encrypt
  prefixSize = discoverPrefixSize blockSize encrypt
  prefixPadLength = blockSize - (prefixSize `mod` blockSize)
  prefixPad = replicate prefixPadLength 0
  -- Wrapped encryption function which drops the padded blocks,
  -- reducing the problem to C12. *Abstraction for the win.*
  encrypt' plaintext =
    drop (prefixSize + prefixPadLength) $
    encrypt (prefixPad ++ plaintext)

----------------------------------------------------------------

prop_discoverPrefixSize_correct :: QC.Property
prop_discoverPrefixSize_correct =
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
    QC.forAll QC.arbitrary $ \(QC.NonNegative prefixSize) ->
    QC.forAll QC.arbitrary $ \(QC.NonNegative suffixSize) ->
      -- Use all-zero padding to be sure to trigger collision with
      -- plaintext chosen by @discoverPrefixSize@.
      let prefix = replicate prefixSize 0
          suffix = replicate suffixSize 0
          encrypt = pkcs7Pad blockSize . (prefix ++) . (++ suffix)
       in discoverPrefixSize blockSize encrypt == prefixSize

prop_breakEcbSuffix_correct :: QC.Property
prop_breakEcbSuffix_correct =
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
    QC.forAll QC.arbitrary $ \prefix ->
    QC.forAll QC.arbitrary $ \suffix ->
      let encrypt = pkcs7Pad blockSize . (prefix ++) . (++ suffix) in
      breakEcbSuffix encrypt == suffix

-- | Test that 'breakEcbSuffix' discovers the right suffix and show it
-- (surprise, it's Vanilla Ice lyrics).
discoverSecretSuffix :: IO ()
discoverSecretSuffix = do
  let suffix = breakEcbSuffix ecbEncryptionOracle
  assert "Suffix correct" (suffix == C12.secretSuffix) $
    printf "Secret suffix: %s\n" (rawToString suffix)

main :: IO ()
main = do
  QC.quickCheck prop_discoverPrefixSize_correct
  QC.quickCheck prop_breakEcbSuffix_correct
  discoverSecretSuffix
