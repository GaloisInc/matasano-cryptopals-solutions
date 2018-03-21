module Set1Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

import Set1.C1

spec :: Spec
spec = do
  describe "hex enc/decode" $ do
    modifyMaxSuccess (const 1000) $ do
      it "satisfies identity property" $ do
        property prop_hex_encode_decode_id

  describe "base64 enc/decode" $ do
    modifyMaxSuccess (const 1000) $ do
      it "satisfies identity property" $ do
        property prop_base64_encode_decode_id

prop_hex_encode_decode_id :: [Byte] -> Bool
prop_hex_encode_decode_id bs = (decodeHex $ encodeHex bs) == bs

prop_base64_encode_decode_id :: [Byte] -> Bool
prop_base64_encode_decode_id bs = (decodeBase64 $ encodeBase64 bs) == bs

