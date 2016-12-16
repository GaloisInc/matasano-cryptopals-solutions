{-# LANGUAGE ScopedTypeVariables #-}
{-

### Implement CTR, the stream cipher mode

The string:

    L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==

... decrypts to something approximating English in CTR mode, which is an
AES block cipher mode that turns AES into a stream cipher, with the
following parameters:

          key=YELLOW SUBMARINE
          nonce=0
          format=64 bit unsigned little endian nonce,
                 64 bit little endian block count (byte count / 16)

CTR mode is very simple.

Instead of encrypting the plaintext, CTR mode encrypts a running
counter, producing a 16 byte block of keystream, which is XOR'd against
the plaintext.

For instance, for the first 16 bytes of a message with these parameters:

    keystream = AES("YELLOW SUBMARINE",
                    "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")

... for the next 16 bytes:

    keystream = AES("YELLOW SUBMARINE",
                    "\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00")

... and then:

    keystream = AES("YELLOW SUBMARINE",
                    "\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00")

CTR mode does not require padding; when you run out of plaintext, you
just stop XOR'ing keystream and stop generating keystream.

Decryption is identical to encryption. Generate the same keystream, XOR,
and recover the plaintext.

Decrypt the string at the top of this function, then use your CTR
function to encrypt and decrypt other things.

### This is the only block cipher mode that matters in good code. {.panel-title}

Most modern cryptography relies on CTR mode to adapt block ciphers into
stream ciphers, because most of what we want to encrypt is better
described as a stream than as a sequence of blocks. Daniel Bernstein
once quipped to Phil Rogaway that good cryptosystems don't need the
"decrypt" transforms. Constructions like CTR are what he was talking
about.

-}

module Set3.C18 ( ctrMode ) where

import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2

ctrMode :: Int -> Word64 -> (Raw -> Raw) -> (Raw -> Raw)
ctrMode blockSize nonce hash plaintext = ciphertext
  where
  -- We make the plaintext length a multiple of the block size (using
  -- padding), so that we don't have to treat the last block
  -- specially. We then truncate the ciphertext at the end, back down
  -- to the original size of the plaintext.
  blocks = chunks blockSize (pkcs7Pad blockSize plaintext)
  ctrs =
    [ littleEndianBytes nonce ++ littleEndianBytes i | i <- [(0::Word64)..] ]
  ciphertexts = [ xors (hash ctr) block | (ctr, block) <- zip ctrs blocks ]
  ciphertext = take (length plaintext) $ concat ciphertexts

-- This probably would have been useful in C1
littleEndianBytes :: (Integral i, FiniteBits i) => i -> [Word8]
littleEndianBytes x = assertBitSizeMultipleOf8 $ bytes
  where
  bits = map (testBit x) [0 .. finiteBitSize x - 1]
  bytes = map mkByte (chunks 8 bits)

  assertBitSizeMultipleOf8 = assert
    ("littleEndianBytes: number bits not a multiple of 8: " ++
     show (finiteBitSize x))
    (finiteBitSize x `mod` 8 == 0)

  -- Make byte from *little endian* bits.
  mkByte :: [Bool] -> Word8
  mkByte [] = 0
  mkByte (b:bs) = (if b then 1 else 0) + 2 * mkByte bs

----------------------------------------------------------------

prop_littleEndianBytes_correct :: QC.Property
prop_littleEndianBytes_correct =
  QC.forAll QC.arbitrary $ \(x :: Word64) ->
    collapse (littleEndianBytes x) == x
  where
  collapse [] = 0
  collapse (b:bs) = fromIntegral b + 256 * collapse bs

main :: IO ()
main = do
  QC.quickCheck prop_littleEndianBytes_correct
  printf "Decrypted example:\n%s\n" (rawToString decryptedExample)
  where
  decryptedExample = ctrMode 16 0 (aes128EcbEncrypt key) ciphertext

  key = stringToRaw "YELLOW SUBMARINE"

  ciphertext =
    base64ToRaw "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
