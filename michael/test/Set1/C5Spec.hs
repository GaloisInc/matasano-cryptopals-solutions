module Set1.C5Spec where

import Test.Hspec
import Test.QuickCheck as QC

import Set1.C1
import Set1.C5

spec :: Spec
spec = do
  describe "encodeHex . iceify . unasciify" $ do
    it "encodes an example input" $ do
      (encodeHex (iceify (unasciify msg)))
      `shouldBe`
      out

msg = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

out = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
