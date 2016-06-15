module Set2.C13 where

import Control.Monad.Identity
import Data.Char
import Data.List (intercalate, delete)
import Data.List.Split -- ( chunksOf )

import Set1 hiding ( ciphertext )
import Set2.C11


exampleInput :: String
exampleInput = "foo=bar&baz=qux&zap=zazzle"

exampleOutput :: [(String,String)]
exampleOutput = [ ("foo","bar")
                 , ("baz","qux")
                 , ("zap","zazzle")
                 ]
test :: Bool
test = parseKV exampleInput == Just exampleOutput
test2 :: Bool
test2 = encodeKV exampleOutput == exampleInput


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

test3 :: Bool
test3 = encodeKV (profile_for "foo@bar.com") == "email=foo@bar.com&uid=10&role=user"

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


adminAccount :: Maybe [(String,String)]
adminAccount = parseKV (asciify (ecbDec frankenstein))
  where
  -- email is 13 characters long, which puts "role=" at the end
  -- of a block boundary.
  email = "buggr@foo.com"
  regularBlocks = chunksOf 16 (oracle email)

  -- We get this block by making a long email string, which pushes
  -- over a block boundary and ends with "admin".
  adminBlocks :: [[Byte]]
  adminBlocks = drop 1 (chunksOf 16 ciphertext)
    where
    -- adminBlockEmail is of length (10 + length "admin")
    adminBlockEmail = replicate 10 'x' ++ "admin"
    ciphertext = oracle adminBlockEmail

  frankenstein :: [Byte]
  frankenstein = concat (init regularBlocks ++ adminBlocks)

isCorrect :: Bool
isCorrect = case adminAccount of
  Just ps -> maybe False
                   ("admin" ==)
                   (lookup "role" ps)
  Nothing -> False

