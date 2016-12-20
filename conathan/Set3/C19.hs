{-

### Break fixed-nonce CTR mode using substitutions

Take your CTR encrypt/decrypt function and fix its nonce value to 0.
Generate a random AES key.

In *successive encryptions* (*not* in one big running CTR stream),
encrypt each line of the base64 decodes of the following, producing
multiple independent ciphertexts:

    SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ==
    Q29taW5nIHdpdGggdml2aWQgZmFjZXM=
    RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ==
    RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4=
    SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk
    T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==
    T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ=
    UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==
    QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU=
    T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl
    VG8gcGxlYXNlIGEgY29tcGFuaW9u
    QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA==
    QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk=
    QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg==
    QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo=
    QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4=
    VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA==
    SW4gaWdub3JhbnQgZ29vZCB3aWxsLA==
    SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA==
    VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg==
    V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw==
    V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA==
    U2hlIHJvZGUgdG8gaGFycmllcnM/
    VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w=
    QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4=
    VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ=
    V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs=
    SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA==
    U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA==
    U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4=
    VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA==
    QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu
    SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc=
    VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs
    WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs=
    SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0
    SW4gdGhlIGNhc3VhbCBjb21lZHk7
    SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw=
    VHJhbnNmb3JtZWQgdXR0ZXJseTo=
    QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4=

(This should produce 40 short CTR-encrypted ciphertexts).

Because the CTR nonce wasn't randomized for each encryption, each
ciphertext has been encrypted against the same keystream. This is very
bad.

Understanding that, like most stream ciphers (including RC4, and
obviously any block cipher run in CTR mode), the actual "encryption" of
a byte of data boils down to a single XOR operation, it should be plain
that:

    CIPHERTEXT-BYTE XOR PLAINTEXT-BYTE = KEYSTREAM-BYTE

And since the keystream is the same for every ciphertext:

    CIPHERTEXT-BYTE XOR KEYSTREAM-BYTE = PLAINTEXT-BYTE (ie, "you don't
    say!")

Attack this cryptosystem piecemeal: guess letters, use expected English
language frequence to validate guesses, catch common English trigrams,
and so on.

### Don't overthink it. {.panel-title}

Points for automating this, but part of the reason I'm having you do
this is that I think this approach is suboptimal.

-}

{- Solution output from 'main':

00000000001111111111222222222233333333334
01234567890123456789012345678901234567890
I have met them at close of day
Coming with vivid faces
From counter or desk among grey
Eighteenth-century houses.
I have passed with a nod of the head
Or polite meaningless words,
Or have lingered awhile and said
Polite meaningless words,
And thought before I had done
Of a mocking tale or a gibe
To please a companion
Around the fire at the club,
Being certain that they and I
But lived where motley is worn:
All changed, changed utterly:
A terrible beauty is born.
That woman's days were spent
In ignorant good will,
Her nights in argument
Until her voice grew shrill.
What voice more sweet than hers
When young and beautiful,
She rode to harriers?
This man had kept a school
And rode our winged horse.
This other his helper and friend
Was coming into his force;
He might have won fame in the end,
So sensitive his nature seemed,
So daring and sweet his thought.
This other man I had dreamed
A drunken, vain-glorious lout.
He had done most bitter wrong
To some who are near my heart,
Yet I number him in the song;
He, too, has resigned his part
In the casual comedy;
He, too, has been changed in his turn,
Transformed utterly:
A terrible beauty is born.

-}

module Set3.C19
  ( encryptC19
  , englishLikeness
  , guessKeyByte
  , solveRepeatedNonceCtrMode
  ) where

import Common
import Set1
import Set2
import Set3.C18 ( ctrMode )

main :: IO ()
main = solveRepeatedNonceCtrMode key rawCipherTexts
  where
    guesses = map (guessKeyByte englishLikeness) rawCipherTextColumns
    -- My first simple heuristic worked for the first 28 chars, but
    -- then started failing as the columns thin out. Adding some
    -- weight to punctuation, and decreasing the weight for numbers,
    -- got me up to 33 chars. Only a few lines are longer than this,
    -- so I solved those manually; knowing the answer is helpful:
    -- https://www.poetryfoundation.org/resources/learning/core-poems/detail/43289
    key = take 33 [ k | (_score, k, _decryption) <- map head guesses ]
          ++ [ 160, 142, 203, 17, 123 ]
    rawCipherTextColumns = transpose rawCipherTexts
    rawCipherTexts = map encryptC19 plainTextsC19

solveRepeatedNonceCtrMode :: Raw -> [Raw] -> IO ()
solveRepeatedNonceCtrMode key rawCipherTexts = do
  putStrLn $ concat [ show (i `div` 10) | i <- [0..40] ]
  putStrLn $ concat [ show (i `mod` 10) | i <- [0..40] ]
  forM_ rawCipherTexts $ \r -> do
    -- To focus on the three longer lines that I can't break
    -- automatically.
    --
    --when (length r >= 34) $
    putStrLn . rawToString $ xors key r

-- | Guess key bytes that xor-decrypt the given raw bytes to "English
-- like" bytes.
--
-- The idea is to pass in the columns in 'rawCipherTextColumns'.
guessKeyByte :: (String -> Double) -> Raw -> [(Double, Word8, String)]
guessKeyByte rank col = reverse . sort . map (\(k,s) -> (rank s, k, s)) $
  [ (k, rawToString $ repeat k `xors` col) | k <- [0..255] ]

-- | A ranking function which I believe is better than @rankC3@ in
-- @Set1.C3@.
englishLikeness :: String -> Double
englishLikeness = sum . map weight
  where
    -- error "TODO: look at early exercises for existing functions like this that I've already implemented!"
  weightedProperties =
    [ ( 1, \c -> isAscii c && not (isControl c) )
    , ( 1, \c -> isAscii c && isLetter c )
    , ( 0.2, \c -> isAscii c && isNumber c )
    , ( 0.5, \c -> isAscii c && isSpace c )
    -- Punctation that makes sense in prose. Using 0.5 here breaks
    -- parts of the initial 28 key chars.
    , ( 0.4, \c -> isAscii c && elem c ".,;:'\"" ) ]
  weight c = sum [ if p c then w else 0 | (w, p) <- weightedProperties ]

encryptC19 :: Base64 -> Raw
encryptC19 =
  ctrMode aesBlockSize nonce (aes128EcbEncrypt key) . base64ToRaw
  where
  nonce = 0
  key = stringToRaw "YELLOW SUBMARINE"

plainTextsC19 :: [Base64]
plainTextsC19 =
  [ "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ=="
  , "Q29taW5nIHdpdGggdml2aWQgZmFjZXM="
  , "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ=="
  , "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4="
  , "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk"
  , "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
  , "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ="
  , "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
  , "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU="
  , "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl"
  , "VG8gcGxlYXNlIGEgY29tcGFuaW9u"
  , "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA=="
  , "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk="
  , "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg=="
  , "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo="
  , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
  , "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA=="
  , "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA=="
  , "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA=="
  , "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg=="
  , "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw=="
  , "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA=="
  , "U2hlIHJvZGUgdG8gaGFycmllcnM/"
  , "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w="
  , "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4="
  , "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ="
  , "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs="
  , "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA=="
  , "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA=="
  , "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4="
  , "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA=="
  , "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu"
  , "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc="
  , "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs"
  , "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs="
  , "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0"
  , "SW4gdGhlIGNhc3VhbCBjb21lZHk7"
  , "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw="
  , "VHJhbnNmb3JtZWQgdXR0ZXJseTo="
  , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
  ]
