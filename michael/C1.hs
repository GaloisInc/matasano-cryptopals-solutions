module C1 where

import Data.Array
import Data.Char
import Data.Maybe (fromJust)
import Data.Word (Word8)

type Byte = Word8

test1 :: (String, String)
test1 = ( "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        , "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        )

isCorrect :: Bool
isCorrect = (convert $ fst test1) == snd test1


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

factorize :: Integral a => a -> a -> [a]
factorize bitWidth = reverse . go
  where
  go total = if total == 0
                then []
                else let (total',modded) = total `divMod` (2^bitWidth)
                      in modded:(go total')

padToWidth :: Int -> a -> [a] -> [a]
padToWidth len v xs = let lenXs = length xs
                       in case len `compare` lenXs of
                            LT -> error "input is longer than desired length"
                            EQ -> xs
                            GT -> replicate (len-lenXs) v ++ xs

unfactorize :: Integral a => a -> [a] -> a
unfactorize bitWidth = sum . zipWith (*) bases . reverse
  where
  base = 2^bitWidth
  bases = map (base^) [0..]


encodeHex :: [Byte] -> [Char]
encodeHex = concatMap encodeHex'
  where
  encodeHex' :: Byte -> [Char]
  encodeHex' i = map (hexIntToChar !) (padToWidth 2 0 (factorize 4 (fromIntegral i)))

decodeHex :: [Char] -> [Byte]
decodeHex (a:b:rest) = byte : decodeHex rest
  where
  byte = fromIntegral $ unfactorize 4 (map (hexCharToInt !) [a,b])
decodeHex [] = []
decodeHex _ = error "Non even number of hex chars"

encodeBase64 :: [Byte] -> [Char]
encodeBase64 (a:b:c:rest) = encodeBase64' (a,b,c)
                         ++ encodeBase64 rest
  where
  encodeBase64' :: (Byte,Byte,Byte) -> [Char]
  encodeBase64' (a,b,c) = map (base64IntToChar !) (padToWidth 4 0 (factorize 6 i))
    where
    i = unfactorize 8 (map fromIntegral [a,b,c])
encodeBase64 [] = []
encodeBase64 _ = error "Non multiple-of-3-length of bytes"

decodeBase64 :: [Char] -> [Byte]
decodeBase64 (a:b:c:d:rest) = decodeBase64' (a,b,c,d)
                           ++ decodeBase64 rest
  where
  decodeBase64' :: (Char,Char,Char,Char) -> [Byte]
  decodeBase64' (a,b,c,d) = map fromIntegral (padToWidth 3 0 (factorize 8 i))
    where
    i = unfactorize 6 (map (base64CharToInt !) [a,b,c,d])
decodeBase64 [] = []
decodeBase64 _ = error "Non multiple-of-4-length of base64 chars"


convert :: [Char] -> [Char]
convert = encodeBase64 . decodeHex
