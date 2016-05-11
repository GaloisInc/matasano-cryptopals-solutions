module Set1.C7 where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import qualified Data.ByteString as B
import System.Exit

import Set1.C1 hiding ( isCorrect )
import Set1.C5 hiding ( isCorrect,main )

keyBS :: B.ByteString
keyBS = B.pack (unasciify keyString)
  where
  keyString = "YELLOW SUBMARINE"

myECBDecrypt :: [Byte] -> [Byte]
myECBDecrypt = B.unpack . ecbDecrypt aes128 . B.pack
  where
  aes128 = throwCryptoError (cipherInit keyBS) :: AES128

main :: IO ()
main = do
  cipherBytes <- (decodeBase64 . concat . lines) <$> readFile "Set1/7.txt"

  let plainBytes = myECBDecrypt cipherBytes

  mapM_ putStrLn (lines (asciify plainBytes))
