module Set2.C10Spec where

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

import Set1
import Set2.C10

spec :: Spec
spec = do
  describe "CBC enc/decryption" $ do
    modifyMaxSuccess (const 1000) $
      it "satisfies identity property" $ do
        property prop_encrypt_decrypt_id

prop_encrypt_decrypt_id :: [Byte] -> QC.Property
prop_encrypt_decrypt_id plaintext =
  QC.forAll (QC.vector aes128BlockLen) $ \iv ->
    QC.forAll (QC.vector aes128BlockLen) $ \key ->
      plaintext QC.=== myCBCDecrypt iv key (myCBCEncrypt iv key plaintext)

