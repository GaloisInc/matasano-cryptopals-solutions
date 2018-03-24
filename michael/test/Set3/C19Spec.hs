module Set3.C19Spec where

import Test.Hspec

import Set1 ( Byte(..), asciify, unasciify, decodeBase64 )
import Set2.Helpers
import Set3.C19


spec :: Spec
spec = do
  describe "crib-dragging solution" $ do
    it "is correct for seed=99" $ do
      key99
      `shouldBe`
      (take 36 (keystream 99))

key99 = [100,46,77,191,118,217,224,69,133,67,196,116,215,150,240,62,232,34,42,32,202,18,186,238,113,82,97,196,171,182,208,94,216,250,213,174]
