module Set2.C15Spec where

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

import Set1
import Set2.C9
import Set2.C15

spec :: Spec
spec = do
  describe "pkcs_7Unpad_safe" $ do
    it "satisfies identity property" $ do
      property prop_pad_unpad_safe_id

prop_pad_unpad_safe_id :: [Byte] -> QC.Property
prop_pad_unpad_safe_id bs =
  QC.forAll (QC.choose (1,64)) $ \blockLen ->
    Just bs == pkcs_7Unpad_safe (pkcs_7Pad blockLen bs)
