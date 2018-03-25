module Set3.C20Spec where

import Data.Bits ( clearBit )
import Test.Hspec
import Test.Hspec.Core.QuickCheck ( modifyMaxSuccess )
import Test.QuickCheck as QC

import Set1
import Set2.Helpers
import Set3.C19 ( keystream, ciphertexts )
import Set3.C20 ( guessKeystreamFromCiphertexts
                , c20Ciphertexts )


spec :: Spec
spec = do
  describe "guessKeystreamFromCiphertexts" $ do
    modifyMaxSuccess (const 10) $ do
      it "guesses the correct keystream on C19's ciphertexts" $ do
        property (prop_guess_works ciphertexts)

    modifyMaxSuccess (const 5) $ do
      it "guesses the correct keystream on C20's ciphertexts" $ do
        property (prop_guess_works c20Ciphertexts)

prop_guess_works :: (Int -> [[Byte]]) -> Int -> QC.Property
prop_guess_works mkCTs seed =
  map toggleOffCaseBit guessedKeystream
  QC.===
  map toggleOffCaseBit actualKeystream
  where
    toggleOffCaseBit byte = clearBit byte 5
    actualKeystream = take guessLen $ keystream seed
    guessLen = length guessedKeystream
    guessedKeystream = guessKeystreamFromCiphertexts (mkCTs seed)
