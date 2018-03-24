module Set3.C18Spec where

import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck as QC

import Set1 ( Byte(..), asciify, unasciify, decodeBase64 )
import Set3.C18


spec :: Spec
spec = do
  describe "aesCTRMode" $ do
    it "decrypts example string" $ do
      asciify $
        aesCTRModeGo (replicate 8 0) (unasciify "YELLOW SUBMARINE")
                     (decodeBase64 "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==")
      `shouldBe`
      "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby "

    it "satisfies identity property" $ do
      property prop_aesCTRMode_id

prop_aesCTRMode_id :: [Byte] -> QC.Property
prop_aesCTRMode_id msg =
  QC.forAll (QC.vector 8) $ \nonce ->
    QC.forAll (QC.vector 16) $ \key ->
      let enc = aesCTRModeGo nonce key
          dec = aesCTRModeGo nonce key
       in msg QC.=== (dec (enc msg))
