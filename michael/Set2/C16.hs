module Set2.C16 where

import Control.Monad.Identity
import Data.Char
import Data.List ( intercalate, delete, isInfixOf )
import Data.List.Split hiding ( split )
import System.Random

import Set1 hiding ( key )
import Set2.C10 hiding ( main )


{-

AES-128-ECB(prefix || attacker-controlled || suffix, random-key)
Goal: Change suffix.

-}

aes128BlockLen :: Int
aes128BlockLen = 16

header = "comment1=cooking%20MCs;userdata="
footer = ";comment2=%20like%20a%20pound%20of%20bacon"

escape :: String -> String
escape = filter safe
  where
  safe c = c /= ';' && c /= '='

isAdmin :: String -> Bool
isAdmin = isInfixOf ";admin=true;"

key = [0..15]
iv  = [0..15]

encode :: String -> [Byte]
encode str = myCBCEncrypt iv key (unasciify (header ++ escape str ++ footer))

decode :: [Byte] -> String
decode = asciify . myCBCDecrypt iv key

main :: IO ()
main = do
  return ()
