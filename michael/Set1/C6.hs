module Set1.C6 where

import Control.Monad
import Data.Bits (popCount,xor)
import Data.List (sortOn,transpose)
import Data.List.Split (chunksOf)

import Set1.C1 hiding (isCorrect)
import Set1.C3 hiding (isCorrect)
import Set1.C5 hiding (isCorrect,main)

hammingDistance :: [Byte] -> [Byte] -> Int
hammingDistance b1 b2 = sum (map popCount (zipWith (xor) b1 b2))

hammingDistance' :: String -> String -> Int
hammingDistance' s1 s2 = hammingDistance (unasciify s1) (unasciify s2)

---

findBestKeySizes :: [Byte] -> [(Int,Double)]
findBestKeySizes bytes = sortOn snd scoredKeySizes
  where
  scoredKeySizes :: [(Int,Double)]
  scoredKeySizes = flip map [2..40] $ \keySize ->
    let fstKSBytes = take keySize bytes
        sndKSBytes = take keySize $ drop keySize bytes
        thdKSBytes = take keySize $ drop (2*keySize) bytes
        hamming12  = fromIntegral $ hammingDistance fstKSBytes sndKSBytes
        hamming13  = fromIntegral $ hammingDistance fstKSBytes thdKSBytes
        hamming23  = fromIntegral $ hammingDistance sndKSBytes thdKSBytes
        hamming    = (hamming12 + hamming13 + hamming23) / 3
     in (keySize, hamming / (fromIntegral keySize))

correctKey = "Terminator X: Bring the noise"

main :: IO ()
main = do
  ks <- getKeys
  forM_ (zip ks [0..]) (\(k,i) -> putStrLn $ "Key " ++ show i ++ ": " ++ show k)
  if correctKey `elem` ks
     then putStrLn "Contains the correct key!!!"
     else putStrLn "Does not contain the correct key."

getKeys :: IO [String]
getKeys = do
  f <- readFile "6.txt"
  let bytes = decodeBase64 (concat (lines f))
      keySizes = map fst (take 4 (findBestKeySizes bytes))
  forM keySizes $ \keySize -> do
    let transposedBlocks = transpose $ chunksOf keySize bytes
        ranked = map rankedCandidates transposedBlocks
        key = map (fst . head) ranked
        asciiKey = asciify key
    return asciiKey

-- "Tests"
hammingTest :: Bool
hammingTest = 37 == hammingDistance' "this is a test" "wokka wokka!!!"
