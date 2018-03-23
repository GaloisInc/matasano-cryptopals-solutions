module Set3.C17Spec where

import Test.Hspec
import Test.QuickCheck as QC

import Set3.C17


spec :: Spec
spec = do
  describe "padding oracle attack" $ do
    it "decrypts randomly chosen strings" $ do
      property prop_padding_oracle

prop_padding_oracle :: Int -> QC.Property
prop_padding_oracle seed = pt QC.=== plaintext
  where
    (ciphertext, oracle, plaintext) = stuffGo seed
    pt = paddingOracleAttack ciphertext oracle
