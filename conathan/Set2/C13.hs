{-
### ECB cut-and-paste

Write a k=v parsing routine, as if for a structured cookie. The routine
should take:

    foo=bar&baz=qux&zap=zazzle

... and produce:

    {
      foo: 'bar',
      baz: 'qux',
      zap: 'zazzle'
    }

(you know, the object; I don't care if you convert it to JSON).

Now write a function that encodes a user profile in that format, given
an email address. You should have something like:

    profile_for("foo@bar.com")

... and it should produce:

    {
      email: 'foo@bar.com',
      uid: 10,
      role: 'user'
    }

... encoded as:

    email=foo@bar.com&uid=10&role=user

Your "profile\_for" function should *not* allow encoding metacharacters
(& and =). Eat them, quote them, whatever you want to do, but don't let
people set their email address to "foo@bar.com&role=admin".

Now, two more easy functions. Generate a random AES key, then:

1.  Encrypt the encoded user profile under the key; "provide" that to
    the "attacker".
2.  Decrypt the encoded user profile and parse it.

Using only the user input to profile\_for() (as an oracle to generate
"valid" ciphertexts) and the ciphertexts themselves, make a role=admin
profile.

-}

module Set2.C13 () where

import qualified Data.List.Split as S
import qualified Test.QuickCheck as QC

import Common
import Set1
import Set2.C9 hiding ( main )

----------------------------------------------------------------

parseProfile :: String -> [(String,String)]
parseProfile profile =
  let eqs = S.splitOn "&" profile in
  [ (k, v) | eq <- eqs, [k, v] <- [S.splitOn "=" eq] ]

-- The @drop 1@ drops an extra leading '&'.
encodeKeyValuePairs :: [(String,String)] -> String
encodeKeyValuePairs = drop 1 . foldl' (\acc (k,v) -> acc++"&"++k++"="++v) ""

profileFor :: String -> String
profileFor email =
  if any (not . flip elem emailChars) email
  then error "profileFor: bad email address: illegal chars!"
  else encodeKeyValuePairs [("email", email), ("uid", "10"), ("role", "user")]

secretKey :: Raw
secretKey = stringToRaw "YELLOW SUBMARINE"

encryptProfile :: String -> Raw
encryptProfile = aes128EcbEncrypt secretKey . pkcs7Pad 16 . stringToRaw

decryptProfile :: Raw -> String
decryptProfile = rawToString . pkcs7Unpad . aes128EcbDecrypt secretKey

-- | Create an encrypted, encoded profile with @role=admin@.
--
-- I'm assuming that it's OK to have repeated keys, and keys other
-- than "email", "uid", and "role". The idea, then, is to get a block
-- that starts with "admin", and append to a block that ends with
-- "role=", and finally a block that ends with "=user<padding>", as a
-- value for the dangling key and padding to make the unpadding work.
--
-- I did not assume that it's OK to have keys without values.
createAdminProfile :: Raw
createAdminProfile =
  -- Compute a prefix that makes "role=" end on a block boundary.

  let minLength = length "email=x@y.&uid=10&role=" in
  -- A pad length with at least 16 chars to work with.
  let padLength = ((minLength `div` 16) + 2) * 16 - minLength in
  let email = "x@y." ++ replicate padLength 'z' in
  let prefixPlaintext = "email="++email++"&uid=10&role=" in
  assert "'role=' ends block"
  (length prefixPlaintext `mod` 16 == 0) $
  let prefix =
        take (length prefixPlaintext) $
        encryptProfile (profileFor email) in

  -- Compute a block that starts with "admin".
  let email' = "x@y." ++ replicate (16 - length "email=x@y.") 'z' ++ "admin" in
  let admin = take 16 $ drop 16 $ encryptProfile (profileFor email') in

  -- Compute a block consisting of "=user<padding>".
  let suffix =
        drop (length prefixPlaintext) $
        -- The 'email' gives us "user<padding>", so shift everything
        -- right by one char.
        encryptProfile (profileFor (email++"z")) in

  prefix ++ admin ++ suffix

----------------------------------------------------------------

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']

emailChars :: [Char]
emailChars = letters ++ ['0'..'9'] ++ ['@','.']

genLetters :: QC.Gen String
genLetters = QC.listOf1 (QC.elements letters)

genKeyValuePairs :: QC.Gen [(String,String)]
genKeyValuePairs = QC.listOf $ (,) <$> genLetters <*> genLetters

prop_parseProfile_roundTrip :: QC.Property
prop_parseProfile_roundTrip =
  QC.forAll genKeyValuePairs $ \kvs ->
    parseProfile (encodeKeyValuePairs kvs) == kvs

prop_encryptDecrypt_roundTrip :: QC.Property
prop_encryptDecrypt_roundTrip =
  QC.forAll QC.arbitrary $ \plaintext ->
    decryptProfile (encryptProfile plaintext) == plaintext

-- A constant boolean test. This definition with '(QC.===)', and the
-- 'QC.quickCheckWith' call below in 'main' are ugly. Is there a
-- better way?
prop_createAdminProfile_correct :: QC.Property
prop_createAdminProfile_correct =
  let encoded = decryptProfile createAdminProfile in
  let kvs = parseProfile encoded in
  QC.once $
  encoded == encodeKeyValuePairs kvs &&
  length (filter ((== "role") . fst) kvs) == 1 &&
  lookup "role" kvs == Just "admin"

main :: IO ()
main = do
  QC.quickCheck prop_parseProfile_roundTrip
  QC.quickCheck prop_encryptDecrypt_roundTrip
  QC.quickCheck prop_createAdminProfile_correct
