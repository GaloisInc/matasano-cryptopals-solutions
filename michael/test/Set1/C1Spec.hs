module Set1.C1Spec where

import Test.Hspec
import Test.QuickCheck as QC

import Set1.C1 as OG
import Set1.C1_orig as NewG

spec :: Spec
spec = do
  describe "OG.convert" $ do
    it "converts example hex to base64" $ do
      (OG.convert $ fst test1) `shouldBe` snd test1

  describe "NewG.convert" $ do
    it "converts example hex to base64" $ do
      (NewG.convert $ fst test1) `shouldBe` snd test1

test1 :: (String, String)
test1 = ( "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        , "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        )
