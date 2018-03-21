module Set1.C2Spec where

import Test.Hspec
import Test.QuickCheck as QC

import Set1.C1
import Set1.C2

spec :: Spec
spec = do
  describe "myXOR" $ do
    it "correctly XOR's two example inputs" $ do
      (myXOR "1c0111001f010100061a024b53535009181c"
                   "686974207468652062756c6c277320657965")
      `shouldBe`
      "746865206b696420646f6e277420706c6179"
