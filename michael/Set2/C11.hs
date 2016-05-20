module Set2.C11 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.List.Split ( chunksOf )
import System.Random
import qualified Test.QuickCheck as QC

import Set1
import Set2.C9 hiding ( main )
import Set2.C10 hiding ( main, prop_encrypt_decrypt_id, runTests )

-- Padded ECB encryption/decryption
myECBEncrypt' :: [Byte] -> [Byte] -> [Byte]
myECBEncrypt' key bs = myECBEncrypt key paddedBs
  where
  blockLen = length key
  paddedBs = pkcs_7Pad blockLen bs

myECBDecrypt' :: [Byte] -> [Byte] -> [Byte]
myECBDecrypt' key = pkcs_7Unpad . myECBDecrypt key

prop_encrypt_decrypt_id :: [Byte] -> QC.Property
prop_encrypt_decrypt_id plaintext =
  QC.forAll (QC.vector aes128BlockLen) $ \key ->
    plaintext QC.=== myECBDecrypt' key (myECBEncrypt' key plaintext)

runTests :: IO ()
runTests = do
  QC.quickCheckWith QC.stdArgs { QC.maxSuccess = 1000 } prop_encrypt_decrypt_id

main :: IO ()
main = do
  g <- getStdGen
  let enc bs = evalRand (randomEnc bs) g
  putStrLn $ "containsRepeat says: " ++ show (containsRepeat enc)

randomEnc :: (RandomGen g)
          => [Byte] -> Rand g [Byte]
randomEnc bs = do
  key <- replicateM 16 getRandom

  padFrontLen <- getRandomR (5,10)
  padBackLen <- getRandomR (5,10)
  padFront <- replicateM padFrontLen getRandom
  padBack <- replicateM padBackLen getRandom
  let paddedBs = padFront ++ bs ++ padBack

  useECB <- getRandom
  if useECB
     then return (myECBEncrypt' key paddedBs)
     else do
       iv <- replicateM 16 getRandom
       return (myCBCEncrypt iv key paddedBs)

containsRepeat :: ([Byte] -> [Byte]) -> Bool
containsRepeat enc =
  let input = replicate (blockLen*4) 0
      output = enc input
      chunks = chunksOf blockLen output
   in (chunks !! 1) == (chunks !! 2)

blockLen :: Int
blockLen = 16
