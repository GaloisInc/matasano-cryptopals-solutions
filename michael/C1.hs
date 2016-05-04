module C1 where

import Data.Array
import Data.Char
import Data.Maybe (fromJust)

test1 :: (String, String)
test1 = ( "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        , "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        )

isCorrect :: Bool
isCorrect = (convert $ fst test1) == snd test1


hexAlphabet :: [Char]
hexAlphabet = ['0'..'9'] ++ ['a'..'f']

hexAlphabetArray :: Array Char Int
hexAlphabetArray = array (min_char,max_char) (zip hexAlphabet [0..])
  where
  min_char = minimum hexAlphabet
  max_char = maximum hexAlphabet
  len = length hexAlphabet - 1

base64Alphabet :: [Char]
base64Alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

base64AlphabetArray :: Array Int Char
base64AlphabetArray = array (0,len) (zip [0..] base64Alphabet)
  where
  len = length base64Alphabet - 1


conv :: (Char,Char,Char) -> (Char,Char)
conv (a,b,c) = (d,e)
  where
  [a_i,b_i,c_i] = map (hexAlphabetArray !) [a,b,c]
  number = 256*a_i + 16*b_i + c_i
  (d_i,e_i) = number `divMod` 64
  [d,e] = map (base64AlphabetArray !) [d_i,e_i]



convert :: [Char] -> [Char]
convert (a:b:c:rest) = d:e:(convert rest)
  where
  (d,e) = conv (a,b,c)
convert [] = []
convert _ = error "Number of characters is not a multiple of 3"

