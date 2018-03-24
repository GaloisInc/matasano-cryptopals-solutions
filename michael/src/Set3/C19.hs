module Set3.C19 where


import Data.Char
import Data.Function ( on )
import Data.List ( sortBy )
import System.Random

import Set1
import Set2.Helpers ( xor' )
import Set3.C18 ( aesCTRMode )


cribDragSort :: String -> [Byte] -> [Byte] -> [(String, Double, Int)]
cribDragSort crb as bs = reverse $ sortBy (compare `on` mid) pairs
  where
    mid (_,x,_) = x
    pairs = zipWith (\str idx -> (str, scoreString str, idx))
                    (go (as `xor'` bs))
                    [0..]
    crib = unasciify crb
    go xored
      | length xored < length crib = []
      | otherwise = (asciify $ xor' (take (length crib) xored) crib)
                  : go (tail xored)


scoreString :: String -> Double
scoreString str = sum (map scoreChar str)
                / fromIntegral (length str)
  where
    scoreChar :: Char -> Double
    scoreChar c
      | isAlpha c = 2
      | isSpace c = 2
      | isMark  c = 1
      | isDigit c = 1
      | otherwise = 0


keystream seed =
   let nonce = replicate 8 0
       key = take 16 (randoms (mkStdGen seed))
    in aesCTRMode nonce key

ciphertexts seed =
  map ((keystream seed `xor'`) . decodeBase64)
   [ "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ=="
   , "Q29taW5nIHdpdGggdml2aWQgZmFjZXM="
   , "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ=="
   , "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4="
   , "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk"
   , "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
   , "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ="
   , "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
   , "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU="
   , "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl"
   , "VG8gcGxlYXNlIGEgY29tcGFuaW9u"
   , "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA=="
   , "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk="
   , "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg=="
   , "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo="
   , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
   , "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA=="
   , "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA=="
   , "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA=="
   , "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg=="
   , "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw=="
   , "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA=="
   , "U2hlIHJvZGUgdG8gaGFycmllcnM/"
   , "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w="
   , "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4="
   , "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ="
   , "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs="
   , "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA=="
   , "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA=="
   , "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4="
   , "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA=="
   , "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu"
   , "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc="
   , "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs"
   , "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs="
   , "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0"
   , "SW4gdGhlIGNhc3VhbCBjb21lZHk7"
   , "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw="
   , "VHJhbnNmb3JtZWQgdXR0ZXJseTo="
   , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
   ]



--------------------------------------------------------------------------------
-- scratch work
--------------------------------------------------------------------------------




-- [6,19]
-- [224,69,133,67,196,116,215,150,240,62,232,34,42]
--
-- [5,23]
-- [217,224,69,133,67,196,116,215,150,240,62,232,34,42,32,202,18,186,238]
--
-- [0,27]
-- [68,46,77,191,118,217,224,69,133,67,196,116,215,150,240,62,232,34,42,32,202,18,186,238,113,82,97,196]
--
-- 0,29
-- [68,46,77,191,118,217,224,69,133,67,196,116,215,150,240,62,232,34,42,32,202,18,186,238,113,82,97,196,171,182]
--
--
-- 0,32
-- [68,46,77,191,118,217,224,69,133,67,196,116,215,150,240,62,232,34,42,32,202,18,186,238,113,82,97,196,171,182,208,94,216]

-- length leaderboard
-- [(37,38),(4,36),(27,34),(29,32),(25,32),(6,32),(28,31),(20,31),(13,31),(2,31),(0,31),(35,30),(33,30),(31,30),(34,29),(32,29),(14,29),(12,29),(8,29),(30,28),(19,28),(16,28),(11,28),(5,28),(9,27),(39,26),(26,26),(24,26),(23,26),(15,26),(3,26),(21,25),(7,25),(1,23),(18,22),(17,22),(36,21),(22,21),(10,21),(38,20)]


cts = ciphertexts 99


printHelpfulSequences key =
  mapM_ print $ map (\(idx,ct) -> (idx, asciify (ct `xor'` key)))
              $ filter (\(idx,ct) -> (length key < length ct))
              $ zip [0..] cts

printAllSequences key =
  mapM_ print $ map (\(idx,ct) -> (idx, asciify (ct `xor'` key)))
              -- $ filter (\(idx,ct) -> (length key < length ct))
              $ zip [0..] cts

