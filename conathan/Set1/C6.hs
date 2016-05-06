module C6 where
{-
### Break repeating-key XOR

### It is officially on, now.

This challenge isn't conceptually hard, but it involves actual
error-prone coding. The other challenges in this set are there to bring
you up to speed. This one is there to **qualify** you. If you can do
this one, you're probably just fine up to Set 6.

[There's a file here.](/static/challenge-data/6.txt) It's been base64'd
after being encrypted with repeating-key XOR.

Decrypt it.

Here's how:

1.  Let KEYSIZE be the guessed length of the key; try values from 2 to
    (say) 40.
2.  Write a function to compute the edit distance/Hamming distance
    between two strings. *The Hamming distance is just the number of
    differing bits.* The distance between:

        this is a test

    and

        wokka wokka!!!

    is **37.** *Make sure your code agrees before you proceed.*

3.  For each KEYSIZE, take the *first* KEYSIZE worth of bytes, and the
    *second* KEYSIZE worth of bytes, and find the edit distance between
    them. Normalize this result by dividing by KEYSIZE.
4.  The KEYSIZE with the smallest normalized edit distance is probably
    the key. You could proceed perhaps with the smallest 2-3 KEYSIZE
    values. Or take 4 KEYSIZE blocks instead of 2 and average the
    distances.
5.  Now that you probably know the KEYSIZE: break the ciphertext into
    blocks of KEYSIZE length.
6.  Now transpose the blocks: make a block that is the first byte of
    every block, and a block that is the second byte of every block, and
    so on.
7.  Solve each block as if it was single-character XOR. You already have
    code to do this.
8.  For each block, the single-byte XOR key that produces the best
    looking histogram is the repeating-key XOR key byte for that block.
    Put them together and you have the key.

This code is going to turn out to be surprisingly useful later on.
Breaking repeating-key XOR ("Vigenere") statistically is obviously an
academic exercise, a "Crypto 101" thing. But more people "know how" to
break it than can *actually break it*, and a similar technique breaks
something much more important.

### No, that's not a mistake.

We get more tech support questions for this challenge than any of the
other ones. We promise, there aren't any blatant errors in this text. In
particular: the "wokka wokka!!!" edit distance really is 37.

[Cryptography Services](https://cryptoservices.github.io/) | [NCC
Group](https://www.nccgroup.trust/us/)
-}

{-
Once the keysize is fixed, the amount of work to break that keysize is
a function of the ciphertext size only (linear or better, depending on
whether you test the whole plaintext or not), since you simply break
(some prefix) of the ciphertext into blocks, and then break the
concatenation of the nth char of all blocks using the single key break
from Challenge 4. So, I'm going to skip keysize discovery to start,
and just try all keys up to a given size.
-}

import Common
import Set1.C1 hiding ( main )
import Set1.C3 hiding ( main )

-- Return ranked list of @(score, key, plaintext-for-key)@.
--
-- I made the score a 'Double' so I could normalize, but the
-- underlying score function is per-character, so there isn't really
-- any need to normalize.
rankMultiCharXors :: Int -> Raw -> [(Double, String, String)]
rankMultiCharXors keyLen ciphertext = [(score, key, plaintext)]
  where
  columnCiphertexts = transpose $ chunks keyLen ciphertext
  -- Only consider the best solution for each key charater.
  columnSolutions :: [(Int, Word8, String)]
  columnSolutions = map (head . rankSingleCharXors) columnCiphertexts
  plaintext = concat $ transpose
    [ columnPlaintext | (_,_,columnPlaintext) <- columnSolutions ]
  key = rawToString [ columnKeyChar | (_,columnKeyChar,_) <- columnSolutions ]
  score =
    sum [ fromIntegral columnScore | (columnScore,_,_) <- columnSolutions ]

-- Break input into chunks of given size.
chunks :: Int -> Raw -> [Raw]
chunks size raw = takeWhile (not . null) . map (take size) $ iterate (drop size) raw

-- Find best key for all keylengths in given range and return
-- score-ranked solutions.
solveMultiCharXor :: [Int] -> Raw -> [(Double, String, String)]
solveMultiCharXor keyLens raw = reverse . sort $
  [ head $ trace ("Trying keyLen = "++show keyLen) rankMultiCharXors keyLen raw | keyLen <- keyLens ]

main :: IO ()
main = do
  ciphertext <- getCiphertext
  forM_ (take 5 $ solveMultiCharXor [1..100] ciphertext) $ \(score, key, plaintext) ->
    printf "Score = %.2f, key = %s, plaintext =\n%s\n" score key plaintext
  where
  -- Downloaded from the challenge page at
  -- http://cryptopals.com/static/challenge-data/6.txt.
  getCiphertext = base64ToRaw . concat . lines <$> readFile "Set1/6.txt"


