module Set1.C7 where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import qualified Data.ByteString as B
import System.Exit

import Set1.C1 hiding ( isCorrect )
import Set1.C5 hiding ( isCorrect,main )

keyString = "YELLOW SUBMARINE"

myECBDecrypt :: [Byte] -> [Byte] -> [Byte]
myECBDecrypt key = B.unpack . ecbDecrypt aes128 . B.pack
  where
  aes128 = throwCryptoError (cipherInit keyBS) :: AES128
  keyBS = B.pack key

myECBEncrypt :: [Byte] -> [Byte] -> [Byte]
myECBEncrypt key = B.unpack . ecbEncrypt aes128 . B.pack
  where
  aes128 = throwCryptoError (cipherInit keyBS) :: AES128
  keyBS = B.pack key

main :: IO ()
main = do
  cipherBytes <- (decodeBase64 . concat . lines) <$> readFile "Set1/7.txt"

  let plainBytes = myECBDecrypt (unasciify keyString) cipherBytes

  mapM_ putStrLn (lines (asciify plainBytes))
