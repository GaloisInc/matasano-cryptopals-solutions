module Set1.C2 where

import Common
import Set1.C1 hiding ( main )

fixedXor :: [Char] -> [Char] -> [Char]
fixedXor x y = rawToBase16 (zipWith xor (base16ToRaw x) (base16ToRaw y))

-- | I've used this function many times (as of C17), so define it with
-- a shorter name.
xors :: Bits a => [a] -> [a] -> [a]
xors = zipWith xor

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
