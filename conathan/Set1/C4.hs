module Set1.C4 where

{- Detect single-character XOR

One of the 60-character strings in this file has been encrypted by single-character XOR.

Find it.

(Your code from #3 should help.)
-}

{-
I wonder where the other lines came from? A secret challenge?
-}

import Common
import Set1.C1 hiding ( main )
import Set1.C3 hiding ( main )

{-
Score = 29, key = 53, plaintext =
Now that the party is jumping

Score = 27, key = 122, plaintext =
Ea NEy2HcAoF2UmCUxe%s)Sv69KQL

Score = 27, key = 103, plaintext =
Iu<HKzhxpik6TxkaNqwS;adxTBXik

Score = 27, key = 101, plaintext =
Kw>JIxjzrki4VzicLsuQ9cfzV@Zki

Score = 27, key = 45, plaintext =
Vwo8lpyl8lp}8hyjla8qk8rmuhqv
-}
main :: IO ()
main = do
  -- Downloaded from the challenge page at
  -- http://cryptopals.com/static/challenge-data/4.txt.
  raws <- map base16ToRaw . lines <$> readFile "Set1/4.txt"
  let rankedAsciis =
        reverse . sort . concat . map (take 5) . map (rankSingleCharXors rankC3) $ raws
  forM_ (take 5 $ rankedAsciis) $ \(score, key, ascii) -> do
    printf "Score = %i, key = %i, plaintext =\n%s\n\n"
      score key ascii
