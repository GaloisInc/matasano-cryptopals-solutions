module Set1.C5 where
{- Implement repeating-key XOR

Here is the opening stanza of an important work of the English
language:

  Burning 'em, if you ain't quick and nimble
  I go crazy when I hear a cymbal

Encrypt it, under the key "ICE", using repeating-key XOR.

In repeating-key XOR, you'll sequentially apply each byte of the key;
the first byte of plaintext will be XOR'd against I, the next C, the
next E, then I again for the 4th byte, and so on.

It should come out to:

  0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272
  a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f

Encrypt a bunch of stuff using your repeating-key XOR
function. Encrypt your mail. Encrypt your password file. Your .sig
file. Get a feel for it. I promise, we aren't wasting your time with
this.
-}

import Common
import Set1.C1 hiding ( main )

repeatingKeyXor :: Raw -> Raw -> Raw
repeatingKeyXor key plaintext = ciphertext
  where
  ciphertext = zipWith xor (concat $ repeat key) plaintext

main :: IO ()
main = do
  forM_ (zip tests [(0::Integer)..]) $ \((key,i,o),k) -> do
    printf "Test %i: 2 * length i = %i, length 0 = %i\n" k (2 * length i) (length o)
    printf "Outcome: %s\n" (if encrypt key i == o then "Passed!" else "Failed!")
  where
  encrypt key i = rawToBase16 $ repeatingKeyXor (stringToRaw key) (stringToRaw i)

  tests =
    [ ( "ICE"
      , "Burning 'em, if you ain't quick and nimble"++"\n"++
        "I go crazy when I hear a cymbal"
      , "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"++
        "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
      )
    ]
