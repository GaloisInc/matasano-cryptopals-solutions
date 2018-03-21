module Set1.C8 where

import Control.Monad
import Data.List.Split (chunksOf)
import qualified Data.Set as S

import Set1.C1

checkRepeat :: Ord a => [a] -> (Bool,[[a]],[[a]])
checkRepeat bs = ( origLen /= dedupLen
                 , chunks
                 , deduped )
  where
  chunks = chunksOf 16 bs
  origLen = length chunks
  deduped = (S.toList (S.fromList chunks))
  dedupLen = length deduped

main :: IO ()
main = do
  cipherBytesList <- (map decodeHex . lines) <$> readFile "Set1/8.txt"
  forM_ cipherBytesList $ \bs -> do
    let (doesRepeat, orig, deduped) = checkRepeat bs
    if doesRepeat
       then do putStrLn "Repeat detected!"
               putStrLn $ "Orig: " ++ show orig
               putStrLn $ "Deduped: " ++ show deduped
       else return ()

