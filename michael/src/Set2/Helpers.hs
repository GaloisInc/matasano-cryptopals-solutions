module Set2.Helpers where

import Data.Bits

affirmate :: Bool -> String
affirmate True  = "Yes"
affirmate False = "No"

xor' :: Bits a => [a] -> [a] -> [a]
xor' = zipWith (xor)

slice :: Int -> Int -> [a] -> [a]
slice begin end = take (end - begin) . drop begin
