module Set1.C4 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char
import Data.List (sortOn)
import Data.Word (Word8)

import Set1.C1
import Set1.C3

main :: IO ()
main = do
  f <- readFile "Set1/4.txt"
  let ls = map decodeHex (lines f)
      allCandidates = concatMap (map snd . rankedCandidates) ls
      sortedBest = sortOn score allCandidates
  mapM_ putStrLn sortedBest
