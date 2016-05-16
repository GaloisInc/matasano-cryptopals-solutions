{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
### An ECB/CBC detection oracle

Now that you have ECB and CBC working:

Write a function to generate a random AES key; that's just 16 random
bytes.

Write a function that encrypts data under an unknown key --- that is, a
function that generates a random key and encrypts under it.

The function should look like:

    encryption_oracle(your-input)
    => [MEANINGLESS JIBBER JABBER]

Under the hood, have the function *append* 5-10 bytes (count chosen
randomly) *before* the plaintext and 5-10 bytes *after* the plaintext.

Now, have the function choose to encrypt under ECB 1/2 the time, and
under CBC the other half (just use random IVs each time for CBC). Use
rand(2) to decide which to use.

Detect the block cipher mode the function is using each time. You should
end up with a piece of code that, pointed at a block box that might be
encrypting ECB or CBC, tells you which one is happening.
-}

{-
The first time I read this I was stumped, thinking that everything
going into the black box was randomly chosen. But in fact, I control
the plaintext. So, this seems to just be a repeat of Challenge 8: find
repeating blocks. But this is even easier, since I can make the input
a long string of the same character, meaning it doesn't matter how
long the pre and post padding are: I will get many repeated blocks in
the middle for ECB mode.
-}

module Set2.C11 where

import qualified System.Random as R
import qualified Test.QuickCheck         as QC
import qualified Test.QuickCheck.Monadic as QC

import Common
import Set1
import Set2.C9 hiding ( main )
import Set2.C10 hiding ( main )

-- | Return encryption of plaintext and whether ecb was used.
encryptionOracle :: Raw -> IO (Raw, Bool)
encryptionOracle plaintext = do
  (prePadLength :: Int)  <- R.randomRIO (5, 10)
  (prePad :: Raw)        <- replicateM prePadLength R.randomIO
  (postPadLength :: Int) <- R.randomRIO (5, 10)
  (postPad :: Raw)       <- replicateM postPadLength R.randomIO
  let paddedPlaintext = prePad ++ plaintext ++ postPad

  (iv :: Raw)  <- replicateM 16 R.randomIO
  (key :: Raw) <- replicateM 16 R.randomIO
  (useEcb :: Bool) <- R.randomIO
  let cipherText =
        if useEcb then
          -- Pad here to ensure plaintext is multiple of block length.
          aes128EcbEncrypt key (pkcs7Pad 16 paddedPlaintext)
        else
          cbcEncrypt 16 iv (aes128EcbEncrypt key) paddedPlaintext
  return (cipherText, useEcb)

-- The encryption oracle runs in IO, but I don't want to force
-- 'detectEcb' to run in IO. So, we return a plaintext, and a function
-- that will classify the corresponding ciphertext.
mkDetectEcb :: Int -> (Raw, Raw -> Bool)
mkDetectEcb blockSize = (plaintext, detectEcb)
  where
  detectEcb ciphertext =
    let (absolute, _relative) = scoreEcbLikelihood blockSize ciphertext in
    -- We only bound the absolute number, since in Challenge 12 the
    -- relative number does not help, since we deal with a cipher that
    -- appends arbitrary amounts of plaintext we don't control.
    absolute >= (numBlocks - nonzeroBlockBound)

  numBlocks :: Num n => n
  numBlocks = 10

  plaintext = replicate (numBlocks * blockSize) 0

  -- In ECB mode, all of the blocks consisting of only zeros will map
  -- to the same ciphertext. So, which blocks might not be all zeros,
  -- given our all-zero plaintext?
  --
  -- - The first block, due to random padding;
  -- - the last block, due to random padding or pkcs#7 padding;
  -- - the second to last block, due to random padding, when pkcs#7
  --   padding adds a full blocksize padding block.
  -- - UPDATE: in Challenge 12, we deal with a cipher that appends more
  --   plaintext, which we don't control, to our provided plaintext. So,
  --   only the blocks we control will repeat.
  --
  -- I.e., at most three blocks generated from our provided plaintext.
  nonzeroBlockBound = 3

----------------------------------------------------------------

prop_detectEcb_correct_1 :: QC.Property
prop_detectEcb_correct_1 =
  QC.monadicIO $ do
    let (plaintext, detectEcb) = mkDetectEcb 16
    (ciphertext, ecbWasUsed) <- QC.run $ encryptionOracle plaintext
    QC.assert $ detectEcb ciphertext == ecbWasUsed

-- | Here we test the kind of cipher used in Challenge 11: arbitrary
-- padding on the back.
prop_detectEcb_correct_2 :: QC.Property
prop_detectEcb_correct_2 =
  QC.forAll QC.arbitrary $ \(QC.Positive blockSize) ->
    QC.forAll QC.arbitrary $ \suffix ->
      let (plaintext, detectEcb) = mkDetectEcb blockSize in
      -- TODO: Block size of 1 causes problems.
      let encrypt = pkcs7Pad (1+blockSize) . (++ suffix) in
      detectEcb (encrypt plaintext) == True

main :: IO ()
main = do
  QC.quickCheck prop_detectEcb_correct_1
  QC.quickCheck prop_detectEcb_correct_2
