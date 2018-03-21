module Set2.C9Spec where

import Test.Hspec
import Test.QuickCheck as QC

import Set1
import Set2.C9

spec :: Spec
spec = do
  describe "pkcs_7Pad" $ do
    it "pads a string" $ do
      pkcs_7Pad 20 (unasciify "YELLOW SUBMARINE")
      `shouldBe`
      unasciify "YELLOW SUBMARINE\x04\x04\x04\x04"

  describe "pkcs_7Unpad" $ do
    it "satisfies identity property" $ do
      property $ prop_pad_unpad_id

prop_pad_unpad_id :: [Byte] -> QC.Property
prop_pad_unpad_id bs =
  QC.forAll (QC.choose (1,64)) $ \blockLen ->
    bs == pkcs_7Unpad (pkcs_7Pad blockLen bs)

