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

main :: IO ()
main = do
  aes128 <- throwCryptoErrorIO (cipherInit keyBS) :: IO AES128

  cipherBytes <- (decodeBase64 . concat . lines) <$> readFile "Set1/7.txt"

  let cipherBS = B.pack cipherBytes
      plainBS = ecbDecrypt aes128 cipherBS
      plainBytes = B.unpack plainBS

  mapM_ putStrLn (lines (asciify plainBytes))
