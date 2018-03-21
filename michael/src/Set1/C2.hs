module Set1.C2 where

import Data.Bits (xor)

import Set1.C1 hiding (isCorrect)


isCorrect :: Bool
isCorrect = (myXOR "1c0111001f010100061a024b53535009181c"
                   "686974207468652062756c6c277320657965")
            == "746865206b696420646f6e277420706c6179"

myXOR :: [Char] -> [Char] -> [Char]
myXOR xs ys = encodeHex (zipWith (xor) (decodeHex xs)
                                       (decodeHex ys))
