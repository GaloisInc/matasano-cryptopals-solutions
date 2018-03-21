module Set1.C3Spec where

import Test.Hspec
import Test.QuickCheck as QC

import Set1.C1
import Set1.C3

spec :: Spec
spec = do
  describe "rankedCandidates" $ do
    it "ranks correct decrytion highest" $ do
      snd (head (rankedCandidates (decodeHex ciphertext)))
      `shouldBe`
      "Cooking MC's like a pound of bacon"

ciphertext :: [Char]
ciphertext = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
