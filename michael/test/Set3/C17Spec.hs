module Set3.C17Spec where

import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck as QC

import Set1 ( asciify )
import Set2.C9 ( pkcs_7Unpad )
import Set3.C17


spec :: Spec
spec = do
  describe "padding oracle attack" $ do
    modifyMaxSuccess (const 10) $ do
      it "decrypts randomly chosen strings" $ do
        property prop_padding_oracle

prop_padding_oracle :: Int -> QC.Property
prop_padding_oracle seed = plaintext QC.=== attackPlaintext
  where
    (ciphertext, oracle, plaintext, iv) = stuffGo seed
    attackOutput = paddingOracleAttack iv ciphertext oracle
    attackPlaintext = asciify (pkcs_7Unpad attackOutput)
