module C4 where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char
import Data.List (sortOn)
import Data.Word (Word8)

import C3

main :: IO ()
main = do
  f <- readFile "4.txt"
  let ls = lines f
      allCandidates = concatMap rankedCandidates ls
      sortedBest = sortOn score allCandidates
  mapM_ putStrLn sortedBest
