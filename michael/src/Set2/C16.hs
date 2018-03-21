module Set2.C16 where

import Data.Char
import Data.List ( intercalate, delete, isInfixOf )
import Data.List.Split hiding ( split )

import Set1 hiding ( key )
import Set2.C10 hiding ( main )
import Set2.Helpers


{-

AES-128-CBC(prefix || attacker-controlled || suffix, random-key)
Goal: Insert the (unescaped) string "admin=true;" into the decryption, by
modifying the ciphertext.

This is totally nuts! It seems that if you know what parts of the ciphertext
you can control, then you can just insert crap into that part of the message!
WAT!?!?!?!?!

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

{-
This is where the magic happens. I insert a null message for simplicity, but the message
could be anything. I know that whatever transformation I apply to the ciphertext block
will get applied to the plaintext block following the ciphertext block I tamper with
(due to the xor in CBC mode). Thus I apply the transformation of xor with my "goal"
block. Since I control the blocks in the middle I know what they will xor against, so
by applying the right transformation I can create the block I desire. Nutso!!
-}
nullCharBlock = replicate 16 '0'
nullBlock  = unasciify nullCharBlock
adminBlock = unasciify ";admin=true;x=yy"

blockMask :: [Byte]
blockMask = xor' nullBlock adminBlock

cipherBlocks :: [[Byte]]
cipherBlocks = chunksOf 16 (encode (nullCharBlock ++ nullCharBlock))

tamperedBlocks :: [[Byte]]
tamperedBlocks = take 2 cipherBlocks
              ++ [ xor' blockMask (cipherBlocks !! 2) ]
              ++ drop 3 cipherBlocks

-- Helpers!
printBlocks :: Show a => [a] -> IO ()
printBlocks = mapM_ print . chunksOf 16

