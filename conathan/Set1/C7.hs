{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Set1.C7 where
{-
### AES in ECB mode

The Base64-encoded content [in this file](/static/challenge-data/7.txt)
has been encrypted via AES-128 in ECB mode under the key

    "YELLOW SUBMARINE".

(case-sensitive, without the quotes; exactly 16 characters; I like
"YELLOW SUBMARINE" because it's exactly 16 bytes long, and now you do
too).

Decrypt it. You know the key, after all.

Easiest way: use OpenSSL::Cipher and give it AES-128-ECB as the cipher.

### Do this with code. {.panel-title}

You can obviously decrypt this using the OpenSSL command-line tool, but
we're having you get ECB working in code for a reason. You'll need it *a
lot* later on, and not just for attacking ECB.
-}

{-
ECB mode is "electronic codebook mode". It's the simple, deprecated
mode where each block of plaintext is encrypted indepedently using the
same key with the block cipher.
-}

import qualified Crypto.Cipher.Types as C
import qualified Crypto.Cipher.AES as C
import qualified Crypto.Error as C
import qualified Data.ByteArray as BA
import Data.String ( fromString )
import Data.Text ( Text, pack, unpack )
import qualified Test.QuickCheck as QC
import Turtle ( ExitCode(..) )
import Turtle.Prelude ( shellStrict )

import Common
import Set1.C1 hiding ( main )

aesBlockSize :: Num a => a
aesBlockSize = 16

-- Might be silly to make this take 'Raw' inputs, since we just
-- convert back to String.
aes128EcbDecryptOpenSsl :: Raw -> Base64 -> IO String
aes128EcbDecryptOpenSsl key ciphertext = do
  -- Command that works in the shell is
  --
  --   openssl enc -d -aes-128-ecb < Set1/7.txt -K 59454c4c4f57205355424d4152494e45
  --
  -- based on
  -- http://crypto-solutions.blogspot.com/2016/02/set-1-challenge-7.html
  -- after much frustration (using '-pass pass:"YELLOW SUBMARINE"'
  -- does not give the same result ??? Indeed, using '-P' to print the
  -- derived key shows that it's different from
  -- 59454c4c4f57205355424d4152494e45 ???).
  --
  -- However, it gets worse: I can use the 'openssl' command with
  -- exact contents of the base64 encoded file, but if I even collapse
  -- the lines (with 'concat . lines') 'openssl' fails to decrypt
  -- it. Passing in the raw data also does not work.
  let cmd :: Text
      cmd = "openssl enc -d -aes-128-ecb -base64 -K " <> pack (rawToBase16 key)
  (exitCode, plaintext) <- shellStrict cmd (fromString ciphertext)
  case exitCode of
    ExitSuccess -> return $ unpack plaintext
    ExitFailure{} -> error "Nonzero exitcode from 'openssl'!"

aes128EcbEncrypt :: Raw -> Raw -> Raw
aes128EcbEncrypt key ciphertext =
  case C.cipherInit (BA.pack key :: BA.Bytes) of
    C.CryptoPassed (cipher :: C.AES128) ->
      BA.unpack $ C.ecbEncrypt cipher (BA.pack ciphertext :: BA.Bytes)
    C.CryptoFailed reason -> error $ "aes128EcbEncrypt: " ++ show reason

aes128EcbDecrypt :: Raw -> Raw -> Raw
aes128EcbDecrypt key ciphertext =
  case C.cipherInit (BA.pack key :: BA.Bytes) of
    C.CryptoPassed (cipher :: C.AES128) ->
      BA.unpack $ C.ecbDecrypt cipher (BA.pack ciphertext :: BA.Bytes)
    C.CryptoFailed reason -> error $ "aes128EcbDecrypt: " ++ show reason
    
----------------------------------------------------------------

prop_aes128_roundtrip =
  QC.forAll (QC.vector aesBlockSize) $ \key ->
    QC.forAll QC.arbitrary $ \(QC.Positive numBlocks) ->
      QC.forAll (QC.vector (aesBlockSize * numBlocks)) $ \plaintext ->
        QC.collect ("numBlocks", numBlocks) $
        plaintext == aes128EcbDecrypt key (aes128EcbEncrypt key plaintext)

compare_cryptonite_to_openssl :: IO ()
compare_cryptonite_to_openssl = do
  base64Ciphertext <- readFile "Set1/7.txt"
  plaintext_1 <- aes128EcbDecryptOpenSsl (stringToRaw "YELLOW SUBMARINE") base64Ciphertext

  ciphertext <- base64ToRaw . concat . lines <$> readFile "Set1/7.txt"
  let plaintext_2 = rawToString $ aes128EcbDecrypt (stringToRaw "YELLOW SUBMARINE") ciphertext

  printf "================================================================\n"
  printf "The openssl plaintext is:\n"
  printf "%s\n" plaintext_1
  printf "\n"
  printf "%s\n" (rawToBase16 . stringToRaw $ plaintext_1)
  printf "\n"
  printf "================================================================\n"
  printf "The cryptonite plaintext is:\n"
  printf "%s\n" plaintext_2
  printf "\n"
  printf "%s\n" (rawToBase16 . stringToRaw $ plaintext_2)
  printf "\n"
  printf "================================================================\n"
  printf "Are the two plaintexts equal: %s\n" (show $ plaintext_1 == plaintext_2)
  printf "Length of plaintexts: %s\n" (show $ map length [plaintext_1, plaintext_2])
  -- The cryptonite version has four trailing 0x04 bytes, which is
  -- apparently ASCII EOT (end of transmission), and used for EOF in
  -- Unix. No idea what's going on here.
  --
  -- Update: this is PKCS#7 padding, as covered in Challenge 9.

main :: IO ()
main = do
  QC.quickCheck  prop_aes128_roundtrip
  -- compare_cryptonite_to_openssl
