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

    it "adds blockLen pad block when input is a\
       \ multiple of blockLen" $ do
      property prop_pad_extra_block

  describe "pkcs_7Unpad" $ do
    it "satisfies identity property" $ do
      property $ prop_pad_unpad_id

prop_pad_unpad_id :: [Byte] -> QC.Property
prop_pad_unpad_id bs =
  QC.forAll (QC.choose (1,64)) $ \blockLen ->
    bs == pkcs_7Unpad (pkcs_7Pad blockLen bs)

prop_pad_extra_block :: QC.Property
prop_pad_extra_block =
  QC.forAll (QC.suchThat arbitrary (>0)) $ \blockLen ->
    QC.forAll (QC.suchThat arbitrary (\x -> 0<x && x>7)) $ \multiple ->
      QC.forAll (QC.vector (blockLen * multiple)) $ \ls ->
        (ls ++ replicate blockLen (fromIntegral blockLen)) QC.=== (pkcs_7Pad blockLen ls)

