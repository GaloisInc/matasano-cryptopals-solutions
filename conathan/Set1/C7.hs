{-# LANGUAGE OverloadedStrings #-}
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
I'm interpreting the above as "use openssl command line program, but
script it instead of directly typing a command line.

ECB mode is "electronic codebook mode". It's the simple, deprecated
mode where each block of plaintext is encrypted indepedently using the
same key with the block cipher.
-}

import Data.Monoid ( (<>) )
import Data.String ( fromString )
import Data.Text ( Text, pack, unpack )
import Turtle ( ExitCode(..) )
import Turtle.Prelude ( shellStrict )

import Common
import Set1.C1 hiding ( main )

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
    
main :: IO ()
main = do
  base64Ciphertext <- readFile "Set1/7.txt"
  putStrLn =<< aes128EcbDecryptOpenSsl (stringToRaw "YELLOW SUBMARINE") base64Ciphertext
