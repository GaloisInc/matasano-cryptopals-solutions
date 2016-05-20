{-

### CBC bitflipping attacks

Generate a random AES key.

Combine your padding code and CBC code to write two functions.

The first function should take an arbitrary input string, prepend the
string:

    "comment1=cooking%20MCs;userdata="

.. and append the string:

    ";comment2=%20like%20a%20pound%20of%20bacon"

The function should quote out the ";" and "=" characters.

The function should then pad out the input to the 16-byte AES block
length and encrypt it under the random AES key.

The second function should decrypt the string and look for the
characters ";admin=true;" (or, equivalently, decrypt, split the string
on ";", convert each resulting string into 2-tuples, and look for the
"admin" tuple).

Return true or false based on whether the string exists.

If you've written the first function properly, it should *not* be
possible to provide user input to it that will generate the string the
second function is looking for. We'll have to break the crypto to do
that.

Instead, modify the ciphertext (without knowledge of the AES key) to
accomplish this.

You're relying on the fact that in CBC mode, a 1-bit error in a
ciphertext block:

-   Completely scrambles the block the error occurs in
-   Produces the identical 1-bit error(/edit) in the next ciphertext
    block.

### Stop and think for a second. {.panel-title}

Before you implement this attack, answer this question: why does CBC
mode have this property?

-}
module Set2.C16 where

import Data.List ( isInfixOf )
import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2.C9 ( pkcs7Pad )
import Set2.C10 ( cbcEncrypt, cbcDecrypt )
import Set2.C15 ( pkcs7SafeUnpad )

-- Don't really care what these are.
key :: Raw
key = [0..15]
iv :: Raw
iv = key

encode :: String -> Raw
encode s =
  cbcEncrypt 16 iv (aes128EcbEncrypt key) . pkcs7Pad 16 . stringToRaw $
  "comment1=cooking%20MCs;userdata=" ++
  escape s ++
  ";comment2=%20like%20a%20pound%20of%20bacon"

escape :: String -> String
escape = filter safe
  where
  safe c = c /= ';' && c /= '='

decode :: Raw -> Maybe String
decode =
  fmap rawToString .
  pkcs7SafeUnpad 16 .
  cbcDecrypt 16 iv (aes128EcbDecrypt key)

isAdmin :: String -> Bool
isAdmin = isInfixOf ";admin=true;"

----------------------------------------------------------------

-- | A ciphertext that decrypts to include the ";admin=true;" string.
--
-- The attack is very simple. In CBC mode, the second ciphertext
-- block, @C1@, is the encryption of @P1 xor C0@, for @P1@ the second
-- plaintext block, and @C0@ the first cipher text block. On
-- decryption, @C1@ is decrypted to @P1 xor C0@, and then this is
-- xored with @C0@ to recover @P1@. So, we replace @C0@ with crafted
-- value @C0'@, s.t.
--
-- > P1 xor C0 xor C0' ==
--
-- How, just solve for @C0'@ by canceling xors:
--
-- > C0' == P1 xor C0 xor <value containing ";admin=true;">
attackedCipherText :: Raw
attackedCipherText = concat $ [c0'] ++ tail blocks
  where
  vanillaCipherText = encode ""
  blocks = chunks 16 vanillaCipherText
  c0 = head blocks
  -- Note that first string is length 32. The second string is padded
  -- with "x"s to be at least 16 bytes long :P
  p1         = stringToRaw (drop 16 "comment1=cooking%20MCs;userdata=")
  adminValue = stringToRaw (take 16 ";admin=true;xxxxxxxxxxxxxxxxxxxx")
  c0' = foldl1 (zipWith xor) [ p1, c0, adminValue ]

----------------------------------------------------------------

prop_attackSucceeded :: QC.Property
prop_attackSucceeded =
  QC.once $ case decode attackedCipherText of
    Nothing -> QC.property $ True
    Just s -> trace ("blocks s: "++ show (chunks 16 s)) $ QC.property (isAdmin s)

main :: IO ()
main = do
  QC.quickCheck prop_attackSucceeded
