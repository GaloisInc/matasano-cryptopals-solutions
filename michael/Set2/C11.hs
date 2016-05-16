module Set2.C11 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import System.Random

import Set1
import Set2.C9 hiding ( main )
import Set2.C10 hiding ( main )


main :: IO ()
main = do
  setStdGen . snd =<< runRandT randMain =<< getStdGen

randomEnc :: (RandomGen g, Monad m)
          => [Byte] -> RandT g m [Byte]
randomEnc bs = do
  key <- replicateM 16 getRandom

  padFrontLen <- getRandomR (5,10)
  let padBackLen = 10 - padFrontLen
  padFront <- replicateM padFrontLen getRandom
  padBack <- replicateM padBackLen getRandom
  let paddedBs = padFront ++ bs ++ padBack

  useECB <- getRandom
  if useECB
     then return (myECBEncrypt key paddedBs)
     else do
       iv <- replicateM 16 getRandom
       return (myCBCDecrypt iv key paddedBs)



randMain :: RandT g IO ()
randMain = do

  liftIO $ putStrLn "ehlloww"
  return ()

