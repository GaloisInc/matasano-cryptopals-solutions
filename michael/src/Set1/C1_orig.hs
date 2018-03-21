module Set1.C1_orig where

import Data.Array
import Data.Char
import Data.Maybe (fromJust)

hexAlphabet :: [Char]
hexAlphabet = ['0'..'9'] ++ ['a'..'f']

hexIntToChar :: Array Int Char
hexIntToChar = array (0,len) (zip [0..] hexAlphabet)
  where
  len = length hexAlphabet - 1

hexCharToInt :: Array Char Int
hexCharToInt = array (min_char,max_char) (zip hexAlphabet [0..])
  where
  min_char = minimum hexAlphabet
  max_char = maximum hexAlphabet

base64Alphabet :: [Char]
base64Alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

base64IntToChar :: Array Int Char
base64IntToChar = array (0,len) (zip [0..] base64Alphabet)
  where
  len = length base64Alphabet - 1

base64CharToInt :: Array Char Int
base64CharToInt = array (min_char,max_char) (zip base64Alphabet [0..])
  where
  min_char = minimum base64Alphabet
  max_char = maximum base64Alphabet

encodeHex :: Int -> (Char,Char,Char)
encodeHex i = (a,b,c)
  where
    (prod,c_i) = i `divMod` 16
    (a_i,b_i) = prod `divMod` 16
    [a,b,c] = map (hexIntToChar !) [a_i,b_i,c_i]

decodeHex :: (Char,Char,Char) -> Int
decodeHex (a,b,c) = 256*a_i + 16*b_i + c_i
  where
  [a_i,b_i,c_i] = map (hexCharToInt !) [a,b,c]

encodeBase64 :: Int -> (Char,Char)
encodeBase64 i = (d,e)
  where
  (d_i,e_i) = i `divMod` 64
  [d,e] = map (base64IntToChar !) [d_i,e_i]

decodeBase64 :: (Char,Char) -> Int
decodeBase64 (d,e) = 64*d_i + e_i
  where
  [d_i,e_i] = map (base64CharToInt !) [d,e]

conv :: (Char,Char,Char) -> (Char,Char)
conv = encodeBase64 . decodeHex



convert :: [Char] -> [Char]
convert (a:b:c:rest) = d:e:(convert rest)
  where
  (d,e) = conv (a,b,c)
convert [] = []
convert _ = error "Number of characters is not a multiple of 3"

