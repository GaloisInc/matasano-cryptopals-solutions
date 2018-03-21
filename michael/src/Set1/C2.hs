module Set1.C2 where

import Data.Bits (xor)

import Set1.C1


myXOR :: [Char] -> [Char] -> [Char]
myXOR xs ys = encodeHex (zipWith (xor) (decodeHex xs)
                                       (decodeHex ys))
