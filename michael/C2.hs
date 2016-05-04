module C2 where

import Data.Bits (xor)

import C1 hiding (isCorrect)


isCorrect :: Bool
isCorrect = (myXOR "1c0111001f010100061a024b53535009181c"
                   "686974207468652062756c6c277320657965")
            == "746865206b696420646f6e277420706c6179"

myXOR :: [Char] -> [Char] -> [Char]
myXOR (a:b:c:rest1) (d:e:f:rest2) = x:y:z:(myXOR rest1 rest2)
  where
  (x,y,z) = encodeHex (decodeHex (a,b,c) `xor` decodeHex (d,e,f))
myXOR [] [] = []
myXOR _  _  = error "Inputs did not have multiple-of-3 lengths"
