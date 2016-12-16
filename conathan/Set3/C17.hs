{-

### The CBC padding oracle

This is the best-known attack on modern block-cipher cryptography.

Combine your padding code and your CBC code to write two functions.

The first function should select at random one of the following 10
strings:

    MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=
    MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=
    MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==
    MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==
    MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl
    MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==
    MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==
    MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=
    MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=
    MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93

... generate a random AES key (which it should save for all future
encryptions), pad the string out to the 16-byte AES block size and
CBC-encrypt it under that key, providing the caller the ciphertext and
IV.

The second function should consume the ciphertext produced by the first
function, decrypt it, check its padding, and return true or false
depending on whether the padding is valid.

<div class="panel panel-warning">

<div class="panel-heading">

### What you're doing here. {.panel-title}

</div>

<div class="panel-body">

This pair of functions approximates AES-CBC encryption as its deployed
serverside in web applications; the second function models the server's
consumption of an encrypted session token, as if it was a cookie.

</div>

</div>

It turns out that it's possible to decrypt the ciphertexts provided by
the first function.

The decryption here depends on a side-channel leak by the decryption
function. The leak is the error message that the padding is valid or
not.

You can find 100 web pages on how this attack works, so I won't
re-explain it. What I'll say is this:

The fundamental insight behind this attack is that the byte 01h is valid
padding, and occur in 1/256 trials of "randomized" plaintexts produced
by decrypting a tampered ciphertext.

02h in isolation is *not* valid padding.

02h 02h *is* valid padding, but is much less likely to occur randomly
than 01h.

03h 03h 03h is even less likely.

So you can assume that if you corrupt a decryption AND it had valid
padding, you know what that padding byte is.

It is easy to get tripped up on the fact that CBC plaintexts are
"padded". *Padding oracles have nothing to do with the actual padding on
a CBC plaintext.* It's an attack that targets a specific bit of code
that handles decryption. You can mount a padding oracle on *any CBC
block*, whether it's padded or not.

-}
module Set3.C17
  ( zeroExtend
  ) where

import Data.Maybe ( isJust )
import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2

plaintexts :: [String]
plaintexts =
  [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
  , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
  , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
  , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
  , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
  , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
  , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
  , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
  , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
  , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
  ]

-- Solution does not use the key, so no reason to generate it
-- randomly.
secretKey :: Raw
secretKey = stringToRaw "YELLOW SUBMARINE"

encrypt :: Raw -> Raw -> Raw
encrypt iv plaintext = ciphertext
  where
  ciphertext =
    cbcEncrypt 16 iv (aes128EcbEncrypt secretKey) (pkcs7Pad 16 plaintext)

paddingIsCorrect :: Raw -> Raw -> Bool
paddingIsCorrect iv ciphertext =
  isJust . pkcs7SafeUnpad 16 $
  cbcDecrypt 16 iv (aes128EcbDecrypt secretKey) ciphertext

zeroExtend :: Num a => Int -> [a] -> [a]
zeroExtend n xs = replicate (n - length xs) 0 ++ xs

-- | The recovered plaintext may be padded; this attack does not
-- remove any such padding.
cbcPaddingOracleAttack :: Int -> (Raw -> Raw -> Bool) -> Raw -> Raw -> Raw
cbcPaddingOracleAttack blockSize paddingOracle iv ciphertext = plaintext
  where
  ciphertexts = chunks blockSize ciphertext
  cipherBlockPairs = zip ([iv] ++ ciphertexts) ciphertexts
  plaintext :: Raw
  plaintext = concatMap breakBlock cipherBlockPairs

  -- Here @(c_0, c_1)@ are two adjacent cipher text blocks -- so
  -- really @(C_n, C_{n+1})@ are better names -- see below for
  -- explanation of the attack.
  breakBlock :: (Raw, Raw) -> Raw
  breakBlock (c_0, c_1) = go []
    where
    go :: Raw -> Raw
    go knownBytes | length knownBytes == blockSize = knownBytes
                  | otherwise = go $ learnByte 0 : knownBytes
      where
      padByte = fromIntegral $ length knownBytes + 1
      -- Here is the tricky part:
      --
      -- Let C_n and C_{n+1} be two adjacent ciphertext blocks (the IV
      -- is considered the first ciphertext block for this
      -- purpose). Then CBC decrypts C_{n+1} as
      --
      -- > C_{n+1} |-> Decrypt (C_{n+1})   `xor` C_n
      --
      -- where
      --
      -- > Decrypt (C_{n+1)) = P_{n+1} `xor` C_n
      --
      -- Now we control the cipher text, so we replace C_n with C'_n,
      -- where C'_n is chosen so that
      --
      -- > (P_{n+1} `xor` C_n) `xor` C'_n
      --
      -- has valid padding, for a pad-byte value of our choice.
      --
      -- Why does this help? Well, if
      --
      -- > (P_{n+1} `xor` C_n) `xor` C'_n
      --
      -- has valid padding, then we know the last pad-byte-many bytes
      -- of that expression are all equal to the pad byte. Hence by
      -- xor-ing on both sides, we have
      --
      -- > final-pad-byte-many-bytes-of P_{n+1) =
      -- >   pad-byte-many-copies-of-pad-byte `xor`
      -- >   final-pad-byte-many-bytes-of (C_n `xor` C'_n)
      --
      --
      -- We know all of the values on the RHS, so we know
      -- pad-byte-many bytes of the plaintext. When the pad byte is
      -- chosen equal to the block size, then we know the entire
      -- plaintext block!
      --
      -- So, how do we do this so we know the padding? Well, first
      -- choose C'_n s.t. the pad-byte is equal to 1, which teaches us
      -- the last byte. Then, knowing the last byte of the plaintext,
      -- we choose C'_n s.t. the pad-byte is equal to 2, which teaches
      -- us the second to last byte of the plaintext. In each case, we
      -- just blindly xor the unknown byte with all 256 possible
      -- bytes, until the padding oracle tells us we choose correctly.
      -- (Pitfall: in the base case where we are trying to set the
      -- pad-byte to 1, we need to check that we didn't simply get
      -- lucky and produce valid padding for a different value! See
      -- use of @perturb@ below.)
      --
      -- Below, we use @c_0@ and @c_1@ for C_n and C_{n+1}, and define
      --
      -- > C'_n = c_0 `xor` delta k
      --
      -- where @k@ is the guessed byte which results in the pad byte
      -- we want.
      delta k = xors
        (zeroExtend blockSize knownBytes)
        (zeroExtend blockSize $ ([k] ++ replicate (length knownBytes) padByte))
      c_0' k = c_0 `xors` delta k
      -- Change the second to last byte of a block-sized chunk of
      -- bytes.
      perturb bytes = bytes `xors` zeroExtend blockSize [1,0]
      learnByte k =
        if paddingOracle (c_0' k) c_1 &&
           -- If we don't know any bytes of this block yet, then it's
           -- possible that two different values could lead to valid
           -- padding -- the value that leads to one pad byte, and
           -- e.g. the value that leads to two pad bytes, if the
           -- second to last byte happens to be 2 -- and so we ensure
           -- that we've set the padding equal to 1 in this case, by
           -- checking that flipping the second to last byte causes
           -- the padding oracle to still accept.
           ( length knownBytes > 0 ||
             -- Of course, if the blockSize is one, then the pad byte
             -- must be 1.
             blockSize == 1 ||
             paddingOracle (perturb $ c_0' k) c_1 ||
             -- This is not really notable: regularly happens for
             -- padded plaintext..
             trace "learnByte: rejected incorrect learned byte!" False
           )
        -- Have
        --
        -- > unknown-plaintext-byte `xor` k = padByte
        then xor k padByte
        else learnByte (k + 1)

----------------------------------------------------------------

prop_paddingCorrect_roundTrip :: QC.Property
prop_paddingCorrect_roundTrip =
  QC.forAll (QC.listOf1 QC.arbitrary) $ \plaintext ->
  QC.forAll (QC.vectorOf 16 QC.arbitrary) $ \iv ->
  paddingIsCorrect iv (encrypt iv plaintext)

prop_cbcPaddingOracleAttack_test :: Raw -> QC.Property
prop_cbcPaddingOracleAttack_test plaintext =
  QC.once $
  QC.forAll (QC.vectorOf 16 QC.arbitrary) $ \iv ->
  let ciphertext = encrypt iv plaintext in
  (pkcs7Unpad $ cbcPaddingOracleAttack 16 paddingIsCorrect iv ciphertext) ==
  plaintext

main :: IO ()
main = do
  QC.quickCheck prop_paddingCorrect_roundTrip
  forM_ plaintexts $ \base64 -> do
    let raw = base64ToRaw base64
    QC.quickCheck (prop_cbcPaddingOracleAttack_test raw)
    printf "plaintext: %s\n" (rawToString raw)
