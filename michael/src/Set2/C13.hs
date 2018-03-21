module Set2.C13 where

import Control.Monad.Identity
import Data.Char
import Data.List (intercalate, delete)
import Data.List.Split

import Set1 hiding ( ciphertext )
import Set2.C11


parseKV :: String -> Maybe [(String,String)]
parseKV str = mapM f pairs
  where
  pairs = splitOn "&" str
  f pair = case splitOn "=" pair of
             [k,v] -> Just (k,v)
             _     -> Nothing

encodeKV :: [(String,String)] -> String
encodeKV pairs = intercalate "&" (map f pairs)
  where
  f (k,v) = k ++ "=" ++ v

deleteMetaChars = delete '&' . delete '='

profile_for :: String -> [(String,String)]
profile_for email = [ ("email", email')
                    , ("uid", "10")
                    , ("role", "user")
                    ]
  where
  email' = deleteMetaChars email

-- Encryption / decryption
keyBytes :: [Byte]
keyBytes = map (fromIntegral . ord) keyString
  where
  keyString = "YELLOW SUBMARINE"

ecbEnc :: [Byte] -> [Byte]
ecbEnc = myECBEncrypt' keyBytes

ecbDec :: [Byte] -> [Byte]
ecbDec = myECBDecrypt' keyBytes

oracle :: String -> [Byte]
oracle = ecbEnc . unasciify . encodeKV . profile_for

