module C2 where

import Data.Bits
import Text.Printf

import C1

fixedXor :: [Char] -> [Char] -> [Char]
fixedXor x y = rawToBase16 (zipWith xor (base16ToRaw x) (base16ToRaw y))

main :: IO ()
main = printf "Test 1 passed? %s!\n" (if test (tests !! 0) then "Yes" else "No")
  where
  test (x,y,out) = fixedXor x y == out
  tests =
    [ ( "1c0111001f010100061a024b53535009181c"
      , "686974207468652062756c6c277320657965"
      , "746865206b696420646f6e277420706c6179"
      )
    ]
