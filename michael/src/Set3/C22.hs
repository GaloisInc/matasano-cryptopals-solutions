module Set3.C22 where


import Control.Concurrent
import Data.Time.Clock.POSIX ( getPOSIXTime )
import System.Random

import Set3.C21


checkBruteForceWords :: IO ()
checkBruteForceWords = do
  (seed,firstOut) <- firstValueFromTimestampSeed
  putStrLn $ "first output: " ++ show firstOut
  seed' <- bruteForceRecoverTimestampSeed firstOut
  putStrLn $ "recovered seed: " ++ show seed'
  putStrLn $ "original seed:  " ++ show seed

firstValueFromTimestampSeed :: IO (Word32, Word32)
firstValueFromTimestampSeed = do
  threadDelay . (*(10^6)) =<< getStdRandom (randomR (40,200))
  timestamp <- round <$> getPOSIXTime
  threadDelay . (*(10^6)) =<< getStdRandom (randomR (40,200))
  let firstRNGOutput = head (mersenneTwister timestamp)
  return (timestamp, firstRNGOutput)

bruteForceRecoverTimestampSeed :: Word32 -> IO Word32
bruteForceRecoverTimestampSeed firstRNGOutput = do
  timestamp <- round <$> getPOSIXTime
  let prevTimestamps = [timestamp, timestamp-1 ..]
      firstOutPuts = map (\x -> (x, head (mersenneTwister x))) prevTimestamps
      (seedTS,_) = head (filter ((==firstRNGOutput) . snd) firstOutPuts)
  return seedTS
