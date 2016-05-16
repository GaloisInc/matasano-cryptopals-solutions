{-

### Byte-at-a-time ECB decryption (Simple)

Copy your oracle function to a new function that encrypts buffers under
ECB mode using a *consistent* but *unknown* key (for instance, assign a
single random key, once, to a global variable).

Now take that same function and have it append to the plaintext, BEFORE
ENCRYPTING, the following string:

    Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
    aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
    dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
    YnkK

### Spoiler alert. {.panel-title}

Do not decode this string now. Don't do it.

Base64 decode the string before appending it. *Do not base64 decode the
string by hand; make your code do it*. The point is that you don't know
its contents.

What you have now is a function that produces:

    AES-128-ECB(your-string || unknown-string, random-key)

It turns out: you can decrypt "unknown-string" with repeated calls to
the oracle function!

Here's roughly how:

1.  Feed identical bytes of your-string to the function 1 at a time ---
    start with 1 byte ("A"), then "AA", then "AAA" and so on. Discover
    the block size of the cipher. You know it, but do this step anyway.
2.  Detect that the function is using ECB. You already know, but do this
    step anyways.
3.  Knowing the block size, craft an input block that is exactly 1 byte
    short (for instance, if the block size is 8 bytes, make "AAAAAAA").
    Think about what the oracle function is going to put in that last
    byte position.
4.  Make a dictionary of every possible last byte by feeding different
    strings to the oracle; for instance, "AAAAAAAA", "AAAAAAAB",
    "AAAAAAAC", remembering the first block of each invocation.
5.  Match the output of the one-byte-short input to one of the entries
    in your dictionary. You've now discovered the first byte of
    unknown-string.
6.  Repeat for the next byte.

### Congratulations. {.panel-title}

This is the first challenge we've given you whose solution will break
real crypto. Lots of people know that when you encrypt something in ECB
mode, you can see penguins through it. Not so many of them can *decrypt
the contents of those ciphertexts*, and now you can. If our experience
is any guideline, this attack will get you code execution in security
tests about once a year.

-}

{-

One thing to note: if n is the block size, then we learn the first
block by using (n-1), then (n-2), down to zero 'A's (or whatever). To
learn the second block, we again use (n-1), then (n-2), down to zero
of any char, since we are now using the known block-one chars to build
the block-two dictionaries.

Also, the description could be better: in the last exercise our oracle
function put random padding on the front and the back; the oracle here
puts padding only on the back, and that padding is fixed. The

    AES-128-ECB(your-string || unknown-string, random-key)

makes everything clear, but the words before that are unclear.

-}

module Set2.C12 where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
-- import qualified System.Random as R
import qualified Test.QuickCheck         as QC
-- import qualified Test.QuickCheck.Monadic as QC
-- 
import Common
import Set1
import Set2.C9 hiding ( main )
import Set2.C11 hiding ( main )

-- From the problem description.
secretSuffix :: Raw
secretSuffix = base64ToRaw $ concat
  [ "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
  , "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
  , "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
  , "YnkK"
  ]

-- Doesn't matter that I know the key, since I don't use it in my
-- decryption.
secretKey :: Raw
secretKey = [0..15]

ecbEncryptionOracle :: Raw -> Raw
ecbEncryptionOracle plaintext =
  aes128EcbEncrypt secretKey (pkcs7Pad 16 $ plaintext ++ secretSuffix)

-- | The prefix is assumed to be of length blocksize-1 bytes. The
-- returned map gives the missing final byte, as a function of the
-- encryption of @prefix++[missing byte]@.
--
-- We could change the dictionary to use a single block as key, which
-- would be more efficient for long suffixes.
mkDictionary :: Int -> Raw -> (Raw -> Raw) -> Map Raw Word8
mkDictionary blockSize prefix encrypt =
  assert "mkDictionary: bad prefix length!" (length prefix == blockSize - 1) $
  Map.fromList [ (take blockSize $ encrypt (prefix++[k]), k)
               | k <- [minBound..maxBound] ]

-- | Learn ECB suffix length by growing prependend plaintext until a
-- block boundary is reached. Assuming PKCS#7 padding is used, the
-- number of blocks will increase by one when the suffix aligns with a
-- block boundary. If the number of blocks at this point is k, and the
-- plaintext length is p, then suffix length is
--
-- > (k-1) * blockSize - p
discoverBlockAndSuffixSize :: (Raw -> Raw) -> (Int, Int)
discoverBlockAndSuffixSize encrypt = go 0
  where
  go p =
    let plaintext = replicate p 0 in
    let plaintext' = 0 : plaintext in
    let l = length (encrypt plaintext) in
    let l' = length (encrypt plaintext') in
    if l == l' then
      go (p+1)
    else
      let blockSize = l' - l in
      -- Here @plaintext'@ caused the ciphertext to grow by one block.
      let suffixSize = l' - blockSize - (p + 1) in
      (blockSize, suffixSize)

-- | Learn the implicit suffix attached to plaintexts by given ECB
-- encryption oracle.
breakEcbSuffix :: (Raw -> Raw) -> Raw
breakEcbSuffix encrypt =
  let (blockSize, suffixSize) = discoverBlockAndSuffixSize encrypt in
  let (plaintext, detectEcb) = mkDetectEcb blockSize in
  if not $ detectEcb (encrypt plaintext) then
    error "breakEcb: not ECB!"
  else
    go blockSize suffixSize
  where
  go :: Int -> Int -> Raw
  go blockSize suffixSize = go' []
    where
    -- Here @knownSuffix@ is the known prefix of secret suffix
    -- appended to plaintexts by the encryption routine.
    --
    -- See block comment below for how @go'@ works.
    go' :: Raw -> Raw
    go' knownSuffix | length knownSuffix == suffixSize = knownSuffix
                    | otherwise =
      let k = length knownSuffix in
      let plaintextLength = blockSize - (k `mod` blockSize) - 1 in
      let blockIndex = (plaintextLength + k) `div` blockSize in
      let plaintext = replicate plaintextLength 0 in
      let prefix = drop (blockIndex * blockSize) (plaintext ++ knownSuffix) in
      let dict = mkDictionary blockSize prefix encrypt in
      let ciphertextBlock = chunks blockSize (encrypt plaintext) !! blockIndex in
      let discoveredSuffixChar = dict Map.! ciphertextBlock in
      go' (knownSuffix ++ [discoveredSuffixChar])

{-

How @go'@ above works
=====================

We want to choose a plaintext which has the effect of pushing the
first unknown char @c@ of the secret suffix to a (known) right block
boundary. Let the (zero-based) index of the known block be @b@. Then the
@b@th block of plaintext is the last block of

> plaintext ++ knownSuffix ++ [c]

The prefix for our dictionary is hence the last @blockSize - 1@ chars
of

> plaintext ++ knownSuffix

So, how long is the plaintext, and what is @b@? We need

> length plaintext + length knownSuffix `mod` blockSize == blockSize - 1

so taking @plaintext@ of minimal length with this property, we have

> length plaintext = blockSize - (length knownSuffix `mod` blockSize) - 1

and the block index @b@ is

> length (plaintext ++ knownSuffix) `div` blockSize

-}

----------------------------------------------------------------

prop_discoverBlockAndSuffixSize_correct :: QC.Property
prop_discoverBlockAndSuffixSize_correct =
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
    QC.forAll QC.arbitrary $ \(QC.Positive succSuffixSize) ->
      let suffixSize = succSuffixSize - 1 in
      let suffix = replicate suffixSize 0 in
      let encrypt = pkcs7Pad blockSize . (++ suffix) in
      -- QC.collect ("(blocksize, suffixSize)", (blockSize, suffixSize)) $
      discoverBlockAndSuffixSize encrypt == (blockSize, suffixSize)

prop_breakEcbSuffix_correct :: QC.Property
prop_breakEcbSuffix_correct =
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
    QC.forAll QC.arbitrary $ \suffix ->
      -- TODO: Block size of 1 causes problems.
      let encrypt = pkcs7Pad blockSize . (++ suffix) in
      breakEcbSuffix encrypt == suffix

-- | Test that 'breakEcbSuffix' discovers the right suffix and show it
-- (surprise, it's Vanilla Ice lyrics).
discoverSecretSuffix :: IO ()
discoverSecretSuffix = do
  let suffix = breakEcbSuffix ecbEncryptionOracle
  assert "Suffix correct" (suffix == secretSuffix) $
    printf "Secret suffix: %s\n" (rawToString suffix)

main :: IO ()
main = do
  QC.quickCheck prop_discoverBlockAndSuffixSize_correct
  QC.quickCheck prop_breakEcbSuffix_correct
  discoverSecretSuffix
