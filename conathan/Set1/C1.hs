module Set1.C1 where

import Common

-- Using the non-URL safe encoding from the RFC.
--
-- https://tools.ietf.org/html/rfc3548.html#section-3

base16Alphabet :: [Char]
base16Alphabet = ['0'..'9'] ++ ['a'..'f']

base64Alphabet :: [Char]
base64Alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

base64Pad :: Char
base64Pad = '='

base16ToBase64 :: [Char] -> [Char]
base16ToBase64 = rawToBase64 . base16ToRaw

base16ToRaw :: [Char] -> Raw
base16ToRaw = compress . map go
  where
  go :: Char -> Word8
  go c = fromList (zip base16Alphabet [0..]) ! c

  compress :: [Word8] -> [Word8]
  compress [] = []
  compress (high:low:xs) = ((high `shiftL` 4) .|. low) : compress xs
  compress _ = error "compress: FIXME: handle odd number of hex digits."

rawToBase16 :: Raw -> [Char]
rawToBase16 (b:bytes) =
  [ base16Alphabet !! fromIntegral x | x <- [b `shiftR` 4, b .&. 0xf] ] ++
  rawToBase16 bytes
rawToBase16 [] = []

rawToBase64 :: Raw -> [Char]
rawToBase64 = go
  where
  go :: [Word8] -> [Char]
  go [] = []
  go (high:middle:low:xs) =
    [ base64Alphabet !! fromIntegral i | i <- indices ] ++ go xs
    where
    -- 24 bits
    all :: Word32
    all = (fromIntegral high `shiftL` 16) .|.
          (fromIntegral middle `shiftL` 8) .|.
          (fromIntegral low)

    indices :: [Word32]
    indices =
      [ (all `shiftR` 18)
      , (all `shiftR` 12) .&. mask
      , (all `shiftR`  6) .&. mask
      , all               .&. mask
      ]

    mask :: Word32
    mask = 0x3f
  go _ = error "rawToBase64: FIXME: handle non multiple of 6 bits."

main :: IO ()
main =
  forM_ (zip tests [(0::Int)..]) $ \((in_, out), i) -> do
    if base16ToBase64 in_ == out then
      printf "Test %i passed!\n" i
    else do
      printf "Test %i failed!\n" i
      printf "Expected then actual:\n"
      printf "%s\n" out
      printf "%s\n" (base16ToBase64 in_)
  where
  tests :: [([Char],[Char])]
  tests =
    [ ( "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
      , "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
      )
    ]
