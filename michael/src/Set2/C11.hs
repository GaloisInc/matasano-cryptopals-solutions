module Set2.C11 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.List.Split ( chunksOf )
import System.Random

import Set1
import Set2.C9
import Set2.C10

-- Padded ECB encryption/decryption
myECBEncrypt' :: [Byte] -> [Byte] -> [Byte]
myECBEncrypt' key bs = myECBEncrypt key paddedBs
  where
  blockLen = length key
  paddedBs = pkcs_7Pad blockLen bs

myECBDecrypt' :: [Byte] -> [Byte] -> [Byte]
myECBDecrypt' key = pkcs_7Unpad . myECBDecrypt key

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
