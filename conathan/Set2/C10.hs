{-# LANGUAGE ScopedTypeVariables #-}
{-
### Implement CBC mode

CBC mode is a block cipher mode that allows us to encrypt
irregularly-sized messages, despite the fact that a block cipher
natively only transforms individual blocks.

In CBC mode, each ciphertext block is added to the next plaintext block
before the next call to the cipher core.

The first plaintext block, which has no associated previous ciphertext
block, is added to a "fake 0th ciphertext block" called the
*initialization vector*, or IV.

Implement CBC mode by hand by taking the ECB function you wrote earlier,
making it *encrypt* instead of *decrypt* (verify this by decrypting
whatever you encrypt to test), and using your XOR function from the
previous exercise to combine them.

[The file here](/static/challenge-data/10.txt) is intelligible
(somewhat) when CBC decrypted against "YELLOW SUBMARINE" with an IV of
all ASCII 0 (\\x00\\x00\\x00 &c)

### Don't cheat. {.panel-title}

Do not use OpenSSL's CBC code to do CBC mode, even to verify your
results. What's the point of even doing this stuff if you aren't going
to learn from it?
-}

{-
- [ ] implement aesEcbEncrypt (seems we only care about aesBlockEncrypt though???)
- [ ] test it with quickcheck
- [ ] implement cbc (using pkcs#7 padding?)
- [ ] what does "ASCII 0" mean? they write '\\x00', but ASCII zero is '\\x30' ???.
-}

module Set2.C10 where

import qualified Crypto.Cipher.Types as C
import qualified Crypto.Cipher.AES as C
import qualified Crypto.Error as C
import qualified Data.ByteArray as BA
import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2.C9 hiding ( main )

-- | Does *not* assume the plaintext is padded.
cbcEncrypt :: Int -> Raw -> (Raw -> Raw) -> (Raw -> Raw)
cbcEncrypt blockSize iv encrypt plaintext = ciphertext
  where
  paddedChunks = chunks blockSize (pkcs7Pad blockSize plaintext)
  -- Encryption is not parallelizable.
  ciphertext = concat . tail $ scanl' xorAndEncrypt iv paddedChunks
  c `xorAndEncrypt` p = encrypt $ zipWith xor c p

-- | Does not assume the plaintext was padded and removes padding.
cbcDecrypt :: Int -> Raw -> (Raw -> Raw) -> (Raw -> Raw)
cbcDecrypt blockSize iv decrypt ciphertext = plaintext
  where
  ciphertexts = chunks blockSize ciphertext
  -- Decryption is parallelizable.
  xoredPlaintexts = map decrypt ciphertexts
  plaintexts = zipWith (zipWith xor) xoredPlaintexts ([iv]++ciphertexts)
  plaintext = pkcs7Unpad $ concat plaintexts

----------------------------------------------------------------

prop_cbc_aes128_encrypt_decrypt_roundtrip =
  QC.forAll (QC.vector blockSize) $ \iv ->
    QC.forAll (QC.vector blockSize) $ \key ->
      QC.forAll QC.arbitrary $ \plaintext ->
        QC.collect ("num blocks", length plaintext `div` blockSize) $
        let ciphertext = cbcEncrypt blockSize iv (aes128EcbEncrypt key) plaintext
            plaintext' = cbcDecrypt blockSize iv (aes128EcbDecrypt key) ciphertext
        in
        plaintext == plaintext'
  where
  blockSize = 16

main :: IO ()
main = do
  decrypt10Txt
  QC.quickCheck prop_cbc_aes128_encrypt_decrypt_roundtrip
  where
  decrypt10Txt = do
    ciphertext <- base64ToRaw . concat . lines <$> readFile "Set2/10.txt"
    let plaintext   = cbcDecrypt 16 iv (aes128EcbDecrypt key) ciphertext
    let ciphertext' = cbcEncrypt 16 iv (aes128EcbEncrypt key) plaintext
    if ciphertext' /= ciphertext then
      error "Renencryption produces different ciphertext!"
    else
      printf "plaintext:\n%s\n" (rawToString plaintext)
    where
    key = stringToRaw "YELLOW SUBMARINE"
    iv = replicate 16 0
