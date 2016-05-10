
import Test.QuickCheck

import C1


prop_hex_encode_decode_id :: [Byte] -> Bool
prop_hex_encode_decode_id bs = (decodeHex $ encodeHex bs) == bs

prop_base64_encode_decode_id :: [Byte] -> Bool
prop_base64_encode_decode_id bs = (decodeBase64 $ encodeBase64 bs) == bs

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_hex_encode_decode_id
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_base64_encode_decode_id

