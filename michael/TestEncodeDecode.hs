
import Test.QuickCheck

import C1


prop_hex_encode_decode_id :: [Byte] -> Bool
prop_hex_encode_decode_id bs = (decodeHex $ encodeHex bs) == bs

prop_base64_encode_decode_id :: [Byte] -> Bool
prop_base64_encode_decode_id bs = (decodeBase64 $ encodeBase64 bs) == bs

main :: IO ()
main = do
  quickCheck prop_hex_encode_decode_id
  quickCheck prop_base64_encode_decode_id
