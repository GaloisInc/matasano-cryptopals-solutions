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
of any char, since we are now using the known block two chars to build
the block two dictionaries.

Also, the description could be better: in the last exercise our oracle
function put random padding on the front and the back; the oracle here
puts padding only on the back, and that padding is fixed. The

    AES-128-ECB(your-string || unknown-string, random-key)

makes everything clear, but the words before that are misleading.
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
  aes128EcbEncrypt secretKey (plaintext ++ secretSuffix)

-- | Discover the block size of an encryption function, assuming it
-- produces ciphertexts that are a multiple of the blocksize. So,
-- e.g., a cipher mode using cipherblock stealing will not reveal the
-- underlying blocksize.
discoverBlockSize :: (Raw -> Raw) -> Int
discoverBlockSize encrypt =
  -- The 20th prime is 71. Nathan "make things overly complicated"
  -- Collins ...
  foldl1' gcd $ map (length . encrypt . flip replicate 0) $
    -- Lots of small numbers, and a few big numbers just in case.
    take 20 primes ++ [250, 1000]
  where
  primes = sieve [2..]
    where
    sieve (p:ns) = p : sieve [ n | n <- ns, n `mod` p /= 0 ]

-- | The prefix is assumed to be of length blocksize-1 bytes. The
-- returned map gives the missing final byte, as a function of the
-- encryption of @prefix++[missing byte]@.
mkDictionary :: Raw -> (Raw -> Raw) -> Map Raw Word8
mkDictionary prefix encrypt =
  Map.fromList [ (encrypt (prefix++[k]), k) | k <- [minBound..maxBound] ]

-- | Learn the implicit suffix attached to plaintexts by given ECB
-- encryption oracle.
breakEcbSuffix :: (Raw -> Raw) -> Raw
breakEcbSuffix encrypt =
  let blockSize = discoverBlockSize encrypt in
  let (plaintext, detectEcb) = mkDetectEcb blockSize in
  if not $ detectEcb (encrypt plaintext) then
    error "breakEcb: not ECB!"
  else
    go blockSize
  where
    go blockSize = undefined

----------------------------------------------------------------

prop_discoverBlockSizeCorrect :: QC.Property
prop_discoverBlockSizeCorrect =
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
    QC.collect ("blocksize", blockSize) $
    discoverBlockSize (pkcs7Pad blockSize) == blockSize

main :: IO ()
main = do
  QC.quickCheck prop_discoverBlockSizeCorrect
